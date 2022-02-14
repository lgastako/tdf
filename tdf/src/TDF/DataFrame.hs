{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.DataFrame
  ( Axes( Axes
        , columnLabels
        , rowLabels
        )
  , DataFrame
  , Something(..)
  , Verbosity(..)
  -- Constructors
  , construct
  , empty
  , fromList
  , fromNativeVec
  , fromScalarList
  , fromSeries
  , fromTexts
  , fromVec
    -- Combinators
  , column
  , extend
  , extendFrom
  , extendWith
  , filterIndexes
  , head
  , map
  , overVec
  , pop
  , rename
  , restrict
  , tail
  -- Optics
  , series
  -- Eliminators
  , asSeries
  , at
  , axes
  , columnVec
  , columns
  , display
  , index
  , indexes
  , isEmpty
  , melt
  , meltSimple
  , ncols
  , ndims
  , nrows
  , onColumn
  , render
  , shape
  , size
  , toFields -- temporarily
  , toList
  , toNativeVec
  , toTexts
  , toVec
  , valueCounts

  -- TODO
  -- , info
  -- , memSize
--  , reindex
--   , under
  ) where

import           TDF.Prelude              hiding ( empty
                                                 , foldr
                                                 , head
                                                 , map
                                                 , toList
                                                 )

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import           Data.Row.Records                ( AllUniqueLabels
                                                 , Extend
                                                 )
import qualified Data.Row.Records     as Rec
import qualified Data.Text            as Text
import qualified Data.Vec.Lazy.X      as Vec
import           TDF.Index                       ( Index )
import qualified TDF.Index            as Index
import           TDF.Options                     ( Options )
import qualified TDF.Options          as Options
import qualified TDF.Types.Table      as Table
import           TDF.Types.ToField               ( ToField )
import qualified TDF.Utils.Dyn        as Dyn
import           TDF.Series                      ( Series )
import qualified TDF.Series           as Series

data DataFrame (n :: Nat) idx a = DataFrame
  { dfIndex :: Index n idx
  , dfData  :: Rec (Map (Series n idx) a)
  } deriving (Generic)

instance ( Forall (Map (Series n idx) a)  NFData
         , NFData idx
         )
  => NFData (DataFrame n idx a)

-- TODO: not the representaton we want but fine for Show... need
--       "real" functions  for rendering
deriving instance ( Forall (Map (Series n idx) a) Show
                  , Show idx
                  ) => Show (DataFrame n idx a)

deriving instance ( Forall (Map (Series n idx) a) Eq
                  , Eq idx
                  ) => Eq (DataFrame n idx a)

data Axes idx = Axes
  { rowLabels    :: [idx]
  , columnLabels :: [Text]
  } deriving (Eq, Generic, Ord,  Show)

data Verbosity
  = Quiet
  | Verbose
  deriving (Bounded, Enum, Generic, Eq, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame
construct :: forall n a.
             ( Forall a Unconstrained1
             , SNatI n
             )
          => Options n Int a
          -> DataFrame n Int a
construct opts = DataFrame dfIndex d
  where
    dfIndex = Options.optIndex opts

    d :: Forall a Unconstrained1 => Rec (Map (Series n Int) a)
    d = Rec.distribute source

    source :: Series n Int (Rec a)
    source = toSeries vecOfRecs

    vecOfRecs :: Vec n (Rec a)
    vecOfRecs = Options.optData opts

toLabelledSeries :: SNatI n
                 => Text
                 -> Vec n a
                 -> Series n Int a
toLabelledSeries label v = Series.construct $ Series.Options
  { optIndex = Index.defaultIntsFor v & fromMaybe (explode "Nahh")
  , optData  = v
  , optName  = Just label
  }

toSeries :: SNatI n => Vec n a -> Series n Int a
toSeries = toLabelledSeries "series"

empty :: DataFrame 'Z Int Empty
empty = construct . Options.fromVec $ Vec.empty

fromList :: ( Forall a Unconstrained1
            , SNatI n
            )
         => [Rec a]
         -> Maybe (DataFrame n Int a)
fromList = fromVec <=< Vec.fromList

fromNativeVec :: forall n t.
                 ( Forall (Rec.NativeRow t) Unconstrained1
                 , Rec.FromNative t
                 , SNatI n
                 )
              => Vec n t
              -> DataFrame n Int (Rec.NativeRow t)
fromNativeVec values = DataFrame
  { dfData  = dfData'
  , dfIndex = Index.defaultIntsFor vecOfRecs
                & fromMaybe (explode "exploding indexes!")
  }
  where
    vecOfRecs :: Vec n (Rec (Rec.NativeRow t))
    vecOfRecs = Vec.map Rec.fromNative values

    recOfVecs :: Rec (Map (Vec n) (Rec.NativeRow t))
    recOfVecs = Rec.distribute vecOfRecs

    dfData' :: Rec (Map (Series n Int) (Rec.NativeRow t))
    dfData' = f' recOfVecs

    f' :: Rec (Map (Vec n) (Rec.NativeRow t))
       -> Rec (Map (Series n Int) (Rec.NativeRow t))
    f' = panic "fromNativeVec.something"

    -- f :: Vec n (Rec (Rec.NativeRow t))
    --   -> Series n Int (Rec.NativeRow t)
    -- f = undefined

--    f :: a -> f a
-- promoteVecToSeries :: forall n a. Vec n a -> Series n Int a
-- promoteVecToSeries = undefined

fromScalarList :: SNatI n
               => [a]
               -> Maybe (DataFrame n Int ("value" .== a))
fromScalarList = fromList . List.map (\x -> #value .== x)

fromSeries :: forall n a. ( SNatI n )
           => Series n Int a
           -> Maybe (DataFrame n Int ("value" .== a))
fromSeries = fromVec . Vec.map (#value .==) . Series.toVec

fromTexts :: forall n a.
             ( Forall a Unconstrained1
             , SNatI n
             )
          => [(Text, [Text])]
          -> Either Text (DataFrame n Int a)
fromTexts = \case
  []     -> Left "Can't construct Frame from empty texts."
  (x:xs) -> Right $ makeDf x xs
    where
      makeDf :: (Text, [Text])
             -> [(Text, [Text])]
             -> DataFrame n Int a
      makeDf _x' _xs' = DataFrame
        { dfIndex = Index.defaultIntsFor theIndexableData
                      & fromMaybe (explode "Splode.1")
        , dfData  = dfData'
        }
        where
          theIndexableData :: Series n Int (Rec a)
          theIndexableData = Rec.sequence dfData'

          dfData' :: Rec (Map (Series n Int) a)
          dfData' = panic "fromTexts.dfData'"

fromVec :: forall n a.
           ( Forall a Unconstrained1
           , SNatI n
           )
        => Vec n (Rec a)
        -> Maybe (DataFrame n Int a)
fromVec v = f <$> Index.defaultIntsFor v
  where
    f :: Index n Int -> DataFrame n Int a
    f idx = DataFrame
      { dfIndex = idx
      , dfData  = dfData'
      }
      where
        dfData' :: Rec (Map (Series n Int) a)
        dfData' = Rec.distribute foo

        foo :: Series n Int (Rec a)
        foo = Series.fromVec v & fromMaybe (explode "DF.fromVec")

-- ================================================================ --
--  Combinators
-- ================================================================ --

column :: forall n r idx k v rest.
          ( Disjoint r rest
          , Forall r Unconstrained1
          , Forall (r .+ rest) Unconstrained1
          , KnownSymbol k
          , SNatI n
          , r ≈ k .== v
          )
       => Label k
       -> DataFrame n idx (r .+ rest)
       -> DataFrame n idx r
column _ = map Rec.restrict

extend :: forall n k v r.
          ( Forall r Unconstrained1
          , Forall (Rec.Extend k v r) Unconstrained1
          , KnownSymbol k
          , SNatI n
          )
       => Label k
       -> v
       -> DataFrame n Int r
       -> DataFrame n Int (Extend k v r)
extend k x = extendWith k (const x)

extendFrom :: forall src dst n v v' a b.
              ( Forall a Unconstrained1
              , Forall b Unconstrained1
              , KnownSymbol src
              , KnownSymbol dst
              , SNatI n
              , (a .! src) ~ v
              , b ~ Extend dst v' a
              )
           => Label src
           -> Label dst
           -> (v -> v')
           -> DataFrame n Int a
           -> DataFrame n Int b
extendFrom src dst f = extendWith dst (\r -> f (r .! src))

extendWith :: forall n k v r.
              ( Forall r Unconstrained1
              , Forall (Extend k v r) Unconstrained1
              , KnownSymbol k
              , SNatI n
              )
           => Label k
           -> (Rec r -> v)
           -> DataFrame n Int r
           -> DataFrame n Int (Extend k v r)
extendWith k f DataFrame {..} = DataFrame
  { dfIndex = dfIndex
  , dfData  = dfData'
  }
  where
    _ = dfData :: Rec (Map (Series n Int) r)

    dfData' :: Rec (Map (Series n Int) (Extend k v r))
    dfData' = Rec.distribute serd'

    serd :: Series n Int (Rec r)
    serd = Rec.sequence dfData

    serd' :: Series n Int (Rec (Extend k v r))
    serd' = fmap (Rec.extend k =<< f) serd

-- TODO: Put the last size variable first to make TypeApplications concise
-- TODO: for all functions that return a dataframe of a new size.
head :: forall m n a.
        ( Forall a Unconstrained1
        , LE m n
        , SNatI n
        , SNatI m
        )
     => DataFrame n Int a
     -> DataFrame m Int a
head DataFrame {..} = DataFrame
  { dfIndex = Index.take dfIndex
  , dfData  = Rec.distribute serd'
  }
  where
    serd :: Series n Int (Rec a)
    serd = Rec.sequence dfData

    serd' :: Series m Int (Rec a)
    serd' = Series.updateVec Vec.take serd

-- TODO are this and over the same?
map :: forall n idx a b.
       ( Forall a Unconstrained1
       , Forall b Unconstrained1
       , SNatI n
       )
    => (Rec a -> Rec b)
    -> DataFrame n idx a
    -> DataFrame n idx b
map f = overVec (Vec.map f)

melt :: forall m n idx a b.
        DataFrame n idx a
     -> DataFrame m idx b
melt = panic "melt"

-- Going to first try where you specify the full set of id cols and the full
-- set of melt cols in full k/v form.

-- If I can get that working then I'll try the same thing but where you just
-- pass the labels instead of the values.

-- Then I'll try making the id cols optional, then the melt cols optional.

-- Then both.

-- Then all of those in one functtion.

meltSimple :: DataFrame n idx a
           -> DataFrame m idx b
meltSimple = panic "simpleMelt"

overVec :: forall m n idx a b.
           ( Forall a Unconstrained1
           , Forall b Unconstrained1
           , SNatI n
           )
        => (Vec n (Rec a) -> Vec m (Rec b))
        -> DataFrame n idx a
        -> DataFrame m idx b
overVec _f DataFrame {} {- {..} -} = DataFrame
  { dfIndex = dfIndex'
  , dfData  = dfData'
  }
  where
    (dfData', dfIndex') = panic "overVec.stuff"
      -- ((,) <$> Rec.distribute <*> reindex')
      --   . f
      --   . Rec.sequence
      --   $ dfData

    -- reindex' :: Vec m (Rec b) -> Index m idx
    -- reindex' = panic "reindex'"

pop :: forall n idx r k v rest.
       ( Disjoint r rest
       , Forall (r .+ rest) Unconstrained1
       , KnownSymbol k
       , SNatI n
       , Series n idx v ~ (Map (Series n idx) (r .+ rest) .! k)
       , r ≈ k .== v
       )
    => Label k
    -> DataFrame n idx (r .+ rest)
    -> (DataFrame n idx rest, Series n idx v)
pop k df = (restrict df, df ^. series k)

rename :: ( Extend k' (sa .! k) (sa .- k) ~ Map (Series n idx) b
          , KnownSymbol k
          , KnownSymbol k'
          , sa ~ Map (Series n idx) a
          , sb ~ Map (Series n idx) b
          )
       => Label k
       -> Label k'
       -> DataFrame n idx a
       -> DataFrame n idx b
rename k k' df = df { dfData = Rec.rename k k' (dfData df) }

restrict :: forall b n idx a.
            ( Forall a Unconstrained1
            , Forall b Unconstrained1
            , Rec.Subset b a
            , SNatI n
            )
         => DataFrame n idx a
         -> DataFrame n idx b
restrict = map Rec.restrict

tail :: forall m n a.
        ( Forall a Unconstrained1
        , LE m n
        , SNatI n
        , SNatI m
        )
     => DataFrame n Int a
     -> DataFrame m Int a
tail DataFrame {..} = DataFrame
  { dfIndex = Index.drop dfIndex
  , dfData  = Rec.distribute serd'
  }
  where
    serd :: Series n Int (Rec a)
    serd = Rec.sequence dfData

    serd' :: Series m Int (Rec a)
    serd' = Series.updateVec Vec.drop serd



-- TODO: merge (however python does it)
--  ~ merge :: DataFrame idx a -> DataFrame idx b -> Extend a b

-- ================================================================ --
--   Optics
-- ================================================================ --

series :: forall n idx r k v rest.
          ( Disjoint r rest
          , (Map (Series n idx) (r .+ rest) .! k) ~ Series n idx v
          , KnownSymbol k
          , r ≈ k .== v
          )
       => Label k
       -> Lens' (DataFrame n idx (r .+ rest)) (Series n idx v)
series k = lens get' set'
  where
    get' df   = dfData df .! k
    set' df s = df { dfData = Rec.update k s (dfData df) }

-- ================================================================ --
--   Eliminators
-- ================================================================ --

asSeries :: forall n a k v.
          ( KnownSymbol k
          , SNatI n
          , ToField v
          , a ≈ k .== v
          , (a .! k) ~ v
          , ((k .== Vec n v) .! k) ~ Vec n ((k .== v) .! k)
          )
       => DataFrame n Int a
       -> Series n Int v
asSeries DataFrame {..} = Series.construct $ Series.Options
  { optIndex = dfIndex
  , optData  = optData'
  , optName  = Just $ Text.intercalate "-" (Rec.labels @a @ToField)
  }
  where
    optData' :: Vec n v
    optData' = fmap snd
      . fmap Rec.unSingleton
      . Series.toVec
      . Rec.sequence
      $ dfData

-- TODO we should really at least eliminate the `Forall a ToField` constraint
-- on things that are just getting the column count -- should be able to get
-- that without rendering them.
at :: ( Forall a Unconstrained1
      , LE n n
      , KnownSymbol k
      , SNatI n
      )
   => Int
   -> Label k
   -> DataFrame n Int a
   -> Maybe (a .! k)
at idx k df = (.! k) <$> lookup idx df

axes :: Forall a ToField
     => DataFrame n idx a
     -> Axes idx
axes df = Axes
  { rowLabels    = Vec.toList . Index.toVec . index $ df
  , columnLabels = columns df
  }

columns :: forall n idx a.
           Forall a ToField
        => DataFrame n idx a
        -> [Text]
columns _ = Rec.labels @a @ToField

--             , (Map (Vec n) a .! k) ~ Vec n (a .! k)


columnVec :: forall n idx a k v r rest.
             ( Disjoint r rest
             , KnownSymbol k
             , a ~ (r .+ rest)
             , v ~ (a .! k)
             , r ≈ k .== v
             , (Map (Series n idx) a .! k) ~ Series n idx v
             )
          => Label k
          -> DataFrame n idx a
          -> Vec n v
columnVec = flip onColumn identity

display :: ( AllUniqueLabels (Map (Vec n) a)
           , Forall (Map (Vec n) a) Something
           , Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           , SNatI n
           )
        => DataFrame n Int a
        -> IO ()
display = putStr
  . Table.render
  . orCrash "display explode"
  . Table.fromHeadedRows
  . List.map Table.Row
  . toTexts

index :: DataFrame n idx a -> Index n idx
index = dfIndex

-- | The index (row labels) of the DataFrame.
indexes :: forall n idx a.
           ( Enum idx
           , Eq idx
           , Forall a Unconstrained1
           , LE n n
           , Num idx
           , SNatI n
           )
        => DataFrame n idx a
        -> Vec n idx
indexes DataFrame {..} = Vec.map fst . Index.index dfIndex $ rows
  where
    rows = panic "indexes.rows"

    -- rows :: Vec n (Rec a)
    -- rows = Rec.sequence dfData

isEmpty :: ( Enum idx
           , Forall a ToField
           )
        => DataFrame n idx a
        -> Bool
isEmpty df
  | ncols df == 0 = True
  | nrows df == 0 = True
  | otherwise     = False

-- TODO: Can we do away with the `Forall a ToField` stuff by substituting
-- a contcreate ToField function from row-types?

-- -- TODO truncate indexes on construction so this doesn't infintie loop
-- memSize :: ( Forall (Map (Vec n) a) NFData
--            , MonadIO m
--            , NFData idx
--            )
--         => DataFrame idx a
--         -> m Word
-- memSize = (liftIO . Data.recursiveSize $!!)

ncols :: Forall a ToField
      => DataFrame n idx a
      -> Int
ncols = length . columns

-- I think this is right?
ndims :: Forall a ToField
      => DataFrame n idx a
      -> Int
ndims df
  | length (columns df) <= 1 = 1
  | otherwise                = 2

nrows :: Enum idx
      => DataFrame n idx a
      -> Int
nrows DataFrame {..} = Index.length dfIndex

onColumn :: forall n idx k v a b r rest.
            ( Disjoint r rest
            , KnownSymbol k
            , (Map (Series n idx) a .! k) ~ Series n idx v
            , a ~ (r .+ rest)
            , r ≈ k .== v
            )
         => Label k
         -> (Vec n v -> b)
         -> DataFrame n idx a
         -> b
onColumn k f = Series.onVec f . view (series k)

render :: forall n idx a.
          ( Forall a ToField
          , Forall a Typeable
          , Forall a Unconstrained1
          , SNatI n
          )
       => DataFrame n idx a
       -> Text
render df@(DataFrame _idx _rv) = Table.render . Table.fromTexts $ headers:rows
  where
    headers = columns df
    rows    = Vec.toList . Vec.map (toFields headers) $ vr

    vr :: Vec n (Rec a)
    vr = panic "render.vr" -- toVec rv

shape :: ( Enum idx
         , Forall a ToField
         )
      => DataFrame n idx a
      -> (Int, Int)
shape = (,) <$> nrows <*> ncols

size :: ( Enum idx
        , Forall a ToField
        )
     => DataFrame n idx a
     -> Int
size = (*) <$> ncols <*> nrows

toList :: ( AllUniqueLabels (Map (Vec n) a)
          , Forall (Map (Vec n) a) Something
          , Forall a Unconstrained1
          , SNatI n
          )
       => DataFrame n Int a
       -> [Rec a]
toList = Vec.toList . toVec

toNativeVec :: forall n t ntv.
               ( AllUniqueLabels (Map (Vec n) ntv)
               , Forall ntv Unconstrained1
               , Forall (Map (Vec n) ntv) Something
               , ToNative t
               , SNatI n
               , ntv ~ NativeRow t
               )
            => DataFrame n Int ntv
            -> Vec n t
toNativeVec = Vec.map Rec.toNative . toVec

toTexts :: forall n a.
           ( AllUniqueLabels (Map (Vec n) a)
           , Forall (Map (Vec n) a) Something
           , Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           , SNatI n
           )
        => DataFrame n Int a
        -> [[Text]]
toTexts df = (headers:)
  . Vec.toList
  . Vec.map f
  $ vec
  where
    vec = toVec df

    headers :: [Text]
    headers = columns df

    f :: Rec a -> [Text]
    f = toFields headers

toVec :: forall n a.
         ( AllUniqueLabels (Map (Vec n) a)
         , Forall (Map (Vec n) a) Something
         , Forall a Unconstrained1
         , SNatI n
         )
      => DataFrame n Int a
      -> Vec n (Rec a)
toVec df = vecOfRecs
  where
    vecOfRecs :: Vec n (Rec a)
    vecOfRecs = Rec.sequence recOfVecs

    recOfSeries :: Rec (Map (Series n Int) a)
    recOfSeries = dfData df

    recOfVecs :: Rec (Map (Vec n) a)
    recOfVecs = recSeriesToRecVec recOfSeries

    recSeriesToRecVec :: Rec (Map (Series n Int) a)
                      -> Rec (Map (Vec n) a)
    recSeriesToRecVec ros = rov
      where
        sor :: Series n Int (Rec a)
        sor = Rec.sequence ros

        vor :: Vec n (Rec a)
        vor = Series.toVec sor

        rov :: Rec (Map (Vec n) a)
        rov = Rec.distribute vor

class Something x where
  _something :: x -> Rec (Map (Vec n) a)

-- instance Something (Rec (Map (Series n idx) a)) where

instance Something a where
  _something = panic "_something"

-- instance Something (Rec (Map (Series n idx) a)) where
--   something = undefined

-- instance (AllUniqueLabels r, Forall r FromJSON) => FromJSON (Rec r) where
--   parseJSON (Object o) = do
--     r <- Rec.fromLabelsA @FromJSON $ \ l -> do x <- o .: (show' l)
--                                                x `seq` pure x
--     r `seq` pure r

--   parseJSON v = typeMismatch msg v
--     where msg = "REC: {" ++ intercalate "," (labels @r @FromJSON) ++ "}"

valueCounts :: forall n idx r k v rest.
               ( r ≈ k .== v
               , Disjoint r rest
               , KnownSymbol k
               , Ord v
               , (Map (Series n idx) (r .+ rest) .! k) ~ Series n idx v
               )
            => Label k
            -> DataFrame n idx (r .+ rest)
            -> Map.Map v Int
valueCounts k = colFoldr k (flip (Map.insertWith (+)) 1) mempty

colFoldr :: forall n idx r k v rest b.
            ( r ≈ k .== v
            , Disjoint r rest
            , KnownSymbol k
            , (Map (Series n idx) (r .+ rest) .! k) ~ Series n idx v
            )
         => Label k
         -> (v -> b -> b)
         -> b
         -> DataFrame n idx (r .+ rest)
         -> b
colFoldr k f z df = Vec.foldr f z ( columnVec k df )

-- ================================================================ --
--  Helpers
-- ================================================================ --

-- TODO this obviously needs to be SOOO much better
lookup :: forall n a.
          ( Forall a Unconstrained1
          , LE n n
          , SNatI n
          )
       => Int
       -> DataFrame n Int a
       -> Maybe (Rec a)
lookup k DataFrame {..} = rMay
  where
    rMay :: Maybe (Rec a)
    rMay = snd <$> find ((k ==) . fst) indexed

    indexed :: Vec n (Int, Rec a)
    indexed = Index.index dfIndex vecOfRecs

    vecOfRecs :: Vec n (Rec a)
    vecOfRecs = Series.toVec foo

    _ = dfData :: Rec (Map (Series n Int) a)

    foo :: Series n Int (Rec a)
    foo = Rec.sequence dfData

toFields :: ( Forall a ToField
            , Forall a Typeable
            )
         => [Text]
         -> Rec a
         -> [Text]
toFields headers r = List.map f headers
  where
    f :: Text -> Text
    f k = Dyn.getValue k dm

    dm = Rec.toDynamicMap r

filterIndexes :: forall m n idx a.
                ( Forall a Unconstrained1
                , Ord idx
                , SNatI n
                )
             => (Vec n idx -> Vec m idx)
             -> DataFrame n idx a
             -> DataFrame m idx a
filterIndexes f DataFrame {..} = DataFrame
  { dfIndex = dfIndex'
  , dfData  = dfData'
  }
  where
    dfIndex' :: Index m idx
    dfIndex' = Index.fromVec . f . Index.toVec $ dfIndex

    dfData' :: Rec (Map (Series m idx) a)
    dfData' = panic "filterIndexes.dfData"

    -- dfData' :: Rec (Map (Vec m) a)
    -- dfData' = restoreByIndexes  @_ @_ @_ @a dfIndex dfIndex' dfData

-- restoreByIndexes :: forall m n idx a.
--                     ( Forall a Unconstrained1
--                     , Ord idx
--                     , SNatI n
--                     )
--                  => Index n idx
--                  -> Index m idx
--                  -> Rec (Map (Vec n) a)
--                  -> Rec (Map (Vec m) a)
-- restoreByIndexes idx idx' r = Rec.distribute
--   . Vec.map (indexed Map.!)
--   . Index.toVec
--   $ idx'
--   where
--     indexed :: Map.Map idx (Rec a)
--     indexed = Map.fromList
--               . Vec.toList
--               . Vec.zipWith (,) (Index.toVec idx)
--               . Rec.sequence
--               $ r

-- under_ :: forall n m a b.
--          ( Forall a Unconstrained1
--          , Forall b Unconstrained1
--          , SNatI n
--          , SNatI m
--          )
--       => (Series n idx (Rec a) -> Series m idx (Rec b))
--       -> Rec (Map (Series n idx) a)
--       -> Rec (Map (Series m idx) b)
-- under_ f = undefined -- Rec.distribute . f . Rec.sequence

-- ================================================================ --
--  TODOs
-- ================================================================ --

-- TODO: iat -- maybe? or explain
-- TODO: explain bool
-- TODO: explain dtypes
-- TODO: explain select_dtypes
-- TODO: explain values replaced by toVector
-- TODO: explain to_numpy replaced by toVector
-- TODO: loc/locLabel

-- swapCols :: forall idx k tmp k' v v' a b rest.
--             ( a ≈ k .== v
--             , b ≈ k .== v'
--             , Disjoint a rest
--             , Disjoint b rest
--             , Rec.Extend
--                           k'
--                           (Rec.Extend
--                              k
--                              (Rec.Extend
--                                 tmp
--                                 v
--                                 ( ((k .== v) .+ rest)
--                                  .- k)
--                               .! k')
--                              (Rec.Extend
--                                 tmp
--                                 v
--                                 (((k .== v) .+ rest)
--                                  .- k)
--                               .- k')
--                            .! tmp)
--                           (Rec.Extend
--                              k
--                              (Rec.Extend
--                                 tmp
--                                 v
--                                 (((k .== v) .+ rest)
--                                  .- k)
--                               .! k')
--                              (Rec.Extend
--                                 tmp
--                                 v
--                                 (((k .== v) .+ rest)
--                                  .- k)
--                               .- k')
--                            .- tmp)
--                         ~ ((k .== v) .+ rest)
--             , ((k .== v) .+ rest) ~ ((k .== v) .+ rest)
--             )
--          => Label k
--          -> Label tmp
--          -> Label k'
--          -> DataFrame n idx (a .+ rest)
--          -> DataFrame n idx (b .+ rest)
-- swapCols k t k' df = rename t k'
--   . rename k' k
--   . rename k t
--   $ df


-- TODO use "Renderable" or something instead of Show for the idxs

-- info :: (Forall a ToField, Show idx)
--      => Verbosity
--      -> DataFrame n idx a
--      -> Text
-- info verbosity = case verbosity of
--   Quiet   -> infoQuiet
--   Verbose -> infoVerbose

-- infoQuiet :: (Forall a ToField, Show idx)
--           => DataFrame n idx a
--           -> Text
-- infoQuiet df = Text.unlines
--   [ showInternalRangeIndex (internalRangeIndex df)
--   , showColIndex (colIndex df)
--   ]

-- infoVerbose :: Show idx => DataFrame n idx a -> Text
-- infoVerbose df = Text.unlines
--   [ showInternalRangeIndex (internalRangeIndex df) <> "\nmore soon\n"
--   , "more soon (verbose)"
--   ]

-- internalRangeIndex :: DataFrame n idx a -> InternalRangeIndex idx
-- internalRangeIndex df =
--   ( length idxs
--   , case idxs of
--       []    -> Nothing
--       (f:xs) -> case reverse xs of
--         [] -> Nothing
--         (l:_) -> Just (f, l)
--   )
--   where
--     idxs = index df

-- showInternalRangeIndex :: Show idx => InternalRangeIndex idx -> Text
-- showInternalRangeIndex (n, Nothing)
--   = "Range index: " <> show n <> " entries."
-- showInternalRangeIndex (n, Just (f, l))
--   = "Range index: " <> show n <> " entries, " <> show f <> " to " <> show l

-- colIndex :: Forall a ToField => DataFrame n idx a -> ColIndex
-- colIndex df = ( length cols
--               , case cols of
--                   []    -> Nothing
--                   (f:xs) -> case reverse xs of
--                               [] -> Nothing
--                               (l:_) -> Just (f, l)
--               )
--   where
--     cols = columns df

-- showColIndex :: ColIndex -> Text
-- showColIndex (n, Nothing)     = "Columns: " <> show n <> " entries."
-- showColIndex (n, Just (f, l)) = "Columns: " <> show n <> " entries, "
--                              <> show f <> " to " <> show l
