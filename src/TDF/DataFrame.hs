{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module TDF.DataFrame
  ( Axes( Axes
        , columnLabels
        , rowLabels
        )
  , DataFrame
  , Verbosity(..)
  , at
  , axes
  , column
  , columnVec
  , columns
  , construct
  , display
  , empty
  , extend
  , extendWith
  , fromList
  , fromNativeVec
  , fromScalarList
  , fromSeries
  , fromVec
  , head
  , head_
  , index
  , indexes
  -- , info
  , isEmpty
  , map
  -- , memSize
  , ndims
  , ncols
  , nrows
  , onColumn
  , onVec
  , over
--  , reindex
  , rename
  , render
  , restrict
  , series
  , shape
  , size
  , tail
  , tail_
  , toFields -- temporarily
  , toList
  , toNativeVec
  , toTexts
  , toVec
  , under
  , valueCounts
  ) where

import           TDF.Prelude              hiding ( empty
                                                 , foldr
                                                 , head
                                                 , map
                                                 , toList
                                                 )

import           Data.Dynamic                    ( Dynamic
                                                 , fromDynamic
                                                 )
import           Data.HashMap.Strict             ( HashMap )
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Row.Records     as Rec
import           Data.String                     ( String )
import qualified Data.Vec.Lazy        as Vec
import           TDF.Index                       ( Index )
import qualified TDF.Index            as Index
import           TDF.Options                     ( Options )
import qualified TDF.Options          as Options
import qualified TDF.Types.Table      as Table
import           TDF.Types.ToField               ( ToField )
import           TDF.Series                      ( Series )
import qualified TDF.Series           as Series

import Data.Row.Internal

data DataFrame (n :: Nat) idx a = DataFrame
  { dfIndex :: Index n idx
  , dfData  :: Rec (Map (Vec n) a)
  } deriving (Generic)

instance ( Forall (Map (Vec n) a)  NFData
         , NFData idx
         )
  => NFData (DataFrame n idx a)

-- TODO: not the representaton we want but fine for Show... need
--       "real" functions  for rendering
deriving instance ( Forall (Map (Vec n) a) Show
                  , Show idx
                  ) => Show (DataFrame n idx a)

deriving instance ( Forall (Map (Vec n) a) Eq
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

-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame
construct :: forall n idx a.
             Forall a Unconstrained1
          => Options n idx a
          -> DataFrame n idx a
construct opts = DataFrame dfIndex d
  where
    dfIndex = Options.optIndex opts

    d :: Forall a Unconstrained1 => Rec (Map (Vec n) a)
    d = Rec.distribute . Options.optData $ opts

empty :: DataFrame 'Z Int Empty
empty = construct . Options.fromVec $ Vec.empty

fromList :: ( Forall a Unconstrained1
            , SNatI n
            )
         => [Rec a]
         -> Maybe (DataFrame n Int a)
fromList = fromVec <=< Vec.fromList

fromNativeVec :: forall n t.
                 ( Rec.FromNative t
                 , Forall (Rec.NativeRow t) Unconstrained1
                 )
              => Vec n t
              -> DataFrame n Int (Rec.NativeRow t)
fromNativeVec values = DataFrame
  { dfData  = Rec.distribute recs
  , dfIndex = dfIndex
  }
  where
    recs :: Vec n (Rec (Rec.NativeRow t))
    recs = Vec.map Rec.fromNative values

    dfIndex = panic "fromNativeVec.dfIndex"

fromScalarList :: SNatI n
               => [a]
               -> Maybe (DataFrame n Int ("value" .== a))
fromScalarList = fromList . List.map (\x -> #value .== x)

fromSeries :: forall n a. ( SNatI n )
           => Series n Int a
           -> Maybe (DataFrame n Int ("value" .== a))
fromSeries = fromVec . Vec.map (#value .==) . Series.toVec

fromVec :: forall n a.
           ( Forall a Unconstrained1
           , SNatI n
           )
        => Vec n (Rec a)
        -> Maybe (DataFrame n Int a)
fromVec recs = f <$> Index.defaultIntsFor recs
  where
    f :: Index n Int -> DataFrame n Int a
    f idx = DataFrame
      { dfIndex = idx
      , dfData  = Rec.distribute recs
      }

-- ================================================================ --
--  Combinators
-- ================================================================ --

extend :: forall n idx k v r.
          ( Forall r Unconstrained1
          , Forall (Rec.Extend k v r) Unconstrained1
          , KnownSymbol k
          )
       => Label k
       -> v
       -> DataFrame n idx r
       -> DataFrame n idx (Rec.Extend k v r)
extend k v = under $ Vec.map (Rec.extend k v)

extendWith :: forall n idx k v r.
              ( Forall r Unconstrained1
              , Forall (Rec.Extend k v r) Unconstrained1
              , KnownSymbol k
              , Monoid v
              )
           => Label k
           -> (Rec r -> v)
           -> DataFrame n idx r
           -> DataFrame n idx (Rec.Extend k v r)
extendWith k f = under $ Vec.map (Rec.extend k =<< f)

-- TODO: Put the last size variable first to make TypeApplications concise
-- TODO: for all functions that return a dataframe of a new size.
head :: forall m n idx a.
        ( Forall a Unconstrained1
        , LE m n
        , SNatI n
        , SNatI m
        )
     => DataFrame n idx a
     -> DataFrame m idx a
head DataFrame {..} = DataFrame
  { dfIndex = Index.take dfIndex
  , dfData  = under_ f dfData
  }
  where
    f :: Forall a Unconstrained1 => Vec n (Rec a) -> Vec m (Rec a)
    f = Vec.take

head_ :: forall n idx a.
         ( Forall a Unconstrained1
         , LE Nat5 n
         , SNatI n
         )
      => DataFrame n idx a
      -> DataFrame Nat5 idx a
head_ = head

map :: forall n idx a b.
       ( Forall a Unconstrained1
       , Forall b Unconstrained1
       , SNatI n
       )
    => (Rec a -> Rec b)
    -> DataFrame n idx a
    -> DataFrame n idx b
map f = onVec (Vec.map f)

onVec :: forall m n idx a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         , SNatI n
         )
      => (Vec n (Rec a) -> Vec m (Rec b))
      -> DataFrame n idx a
      -> DataFrame m idx b
onVec f DataFrame {..} = DataFrame
  { dfIndex = dfIndex'
  , dfData  = dfData'
  }
  where
    (dfData', dfIndex') = ((,) <$> Rec.distribute <*> reindex')
                            . f
                            . Rec.sequence
                            $ dfData

    reindex' :: Vec m (Rec b) -> Index m idx
    reindex' = panic "reindex'"

over :: forall n idx a b.
        ( Forall a Unconstrained1
        , Forall b Unconstrained1
        )
     => (Rec a -> Rec b)
     -> DataFrame n idx a
     -> DataFrame n idx b
over f = under (Vec.map f)

-- -- TODO: questionable to expose? at least with this interface... doing it for
-- --       now for Examples.hs
-- reindex :: [idx']
--         -> DataFrame n idx a
--         -> DataFrame n idx' a
-- reindex = panic "DF.reindex"
-- -- reindex ixs DataFrame {..} = DataFrame dfIndex' dfData
-- --   where
-- --     dfIndex' = Index.fromList ixs

rename :: forall n k k' idx a b.
          ( Forall a Unconstrained1
          , Forall b Unconstrained1
          , KnownSymbol k
          , KnownSymbol k'
          , SNatI n
          , b ~ Rec.Extend k' (a .! k) (a .- k)
          )
       => Label k
       -> Label k'
       -> DataFrame n idx a
       -> DataFrame n idx b
rename k k' DataFrame {..} = DataFrame
  { dfIndex = dfIndex
  , dfData  = dfData'
  }
  where
    _ = dfData :: Rec (Map (Vec n) a)

    dfDataI :: Forall a Unconstrained1 => Vec n (Rec a)
    dfDataI = Rec.sequence dfData

    dfDataI' :: Forall a Unconstrained1 => Vec n (Rec b)
    dfDataI' = Vec.map (Rec.rename k k') dfDataI

    dfData' :: Forall b Unconstrained1 => Rec (Map (Vec n) b)
    dfData' = Rec.distribute dfDataI'

restrict :: forall m n idx a b.
            ( Forall a Unconstrained1
            , Forall b Unconstrained1
            , LE m n
            , Rec.Subset b a
            , SNatI n
            , SNatI m
            )
         => DataFrame n idx a
         -> DataFrame m idx b
restrict DataFrame {..}= DataFrame
  { dfIndex = dfIndex'
  , dfData  = panic "DF.restrict.2" -- dfData'
  }
  where
    _ = dfData :: Rec (Map (Vec n) a)

    dfIndex' :: Index.Index m idx
    dfIndex' = panic "restrict.dfIndex'"

    vrec :: Forall a Unconstrained1 => Vec n (Rec a)
    vrec = Rec.sequence dfData

    vrec' :: ( Forall a Unconstrained1
             , Forall b Unconstrained1
             )
          => Vec n (Rec b)
    vrec' = Vec.map Rec.restrict vrec

    _dfData' :: ( Forall a Unconstrained1
                , Forall b Unconstrained1
                )
             => Rec (Map (Vec n) b)
    _dfData' = Rec.distribute vrec'

-- head :: forall n m idx a.
--         ( Forall a Unconstrained1
--         , LE m n
--         , SNatI n
--         , SNatI m
--         )
--      => DataFrame n idx a
--      -> DataFrame m idx a
-- head DataFrame {..} = DataFrame
--   { dfIndex = Index.take dfIndex
--   , dfData  = under_ f dfData
--   }
--   where
--     f :: Forall a Unconstrained1 => Vec n (Rec a) -> Vec m (Rec a)
--     f = Vec.take

-- head_ :: ( Forall a Unconstrained1
--          , LE Nat5 n
--          , SNatI n
--          )
--       => DataFrame n idx a
--       -> DataFrame Nat5 idx a
-- head_ = head

tail :: forall m n idx a.
        ( Forall a Unconstrained1
        , LE m n
        , SNatI n
        , SNatI m
        )
     => DataFrame n idx a
     -> DataFrame m idx a
tail DataFrame {..} = DataFrame
  { dfIndex = Index.tail dfIndex
  , dfData  = under_ f dfData
  }
  where
    f :: Vec n (Rec a)
      -> Vec m (Rec a)
    f = Vec.drop

tail_ :: ( Forall a Unconstrained1
         , LE Nat5 n
         , SNatI n
         )
      => DataFrame n idx a
      -> DataFrame Nat5 idx a
tail_ = tail

under :: forall m n idx a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vec n (Rec a) -> Vec m (Rec b))
      -> DataFrame n idx a
      -> DataFrame m idx b
under f DataFrame {..} = DataFrame
  { dfIndex = dfIndex'
  , dfData  = dfData'
  }
  where
    (dfIndex', dfData') = underL_ f dfData

-- TODO: merge (however python does it)
--  ~ merge :: DataFrame idx a -> DataFrame idx b -> Extend a b

-- ================================================================ --
--   Eliminators
-- ================================================================ --

-- TODO we should really at least eliminate the `Forall a ToField` constraint
-- on things that are just getting the column count -- should be able to get
-- that without rendering them.
at :: ( Enum idx
      , Eq idx
      , Forall a Unconstrained1
      , LE n n
      , KnownSymbol k
      , Num idx
      , SNatI n
      )
   => idx
   -> Label k
   -> DataFrame n idx a
   -> Maybe (a .! k)
at idx k df = (.! k) <$> lookup idx df

axes :: Forall a ToField
     => DataFrame n idx a
     -> Axes idx
axes df@DataFrame {-{..}-} {} = Axes
  { rowLabels    = panic "axes.1"
  , columnLabels = columns df
  }

columns :: forall n idx a.
           Forall a ToField
        => DataFrame n idx a
        -> [Text]
columns _ = Rec.labels @a @ToField

columnVec :: forall n k idx a.
             ( KnownSymbol k
             , (Map (Vec n) a .! k) ~ Vec n (a .! k)
             )
          => Label k
          -> DataFrame n idx a
          -> Vec n (a .! k)
columnVec = flip onColumn identity

display :: ( Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           , SNatI n
           )
        => DataFrame n idx a
        -> IO ()
display = putStr
  . Table.render
  . fromMaybe explode
  . Table.fromHeadedRows
  . List.map Table.Row
  . toTexts
  where
    explode = panic "display explode"

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
    rows :: Vec n (Rec a)
    rows = Rec.sequence dfData

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

onColumn :: forall n k idx a b.
            ( KnownSymbol k
            , (Map (Vec n) a .! k) ~ (Vec n) (a .! k)
            )
         => Label k
         -> (Vec n (a Rec..! k) -> b)
         -> DataFrame n idx a
         -> b
onColumn k f DataFrame {..} = f $ dfData .! k

render :: forall n idx a.
          ( Forall a ToField
          , Forall a Typeable
          , Forall a Unconstrained1
          , SNatI n
          )
       => DataFrame n idx a
       -> Text
render df@(DataFrame _idx rv) = Table.render . Table.fromTexts $ headers:rows
  where
    headers = columns df
    rows    = Vec.toList . Vec.map (toFields headers) $ vr

    vr :: Vec n (Rec a)
    vr = Rec.sequence rv

series :: forall n idx k r rest v.
          ( r ≈ k .== v
          , Disjoint r rest
          , (Map (Vec n) r .! k) ~ Vec n (r .! k)
          , KnownSymbol k
          , (Map (Vec n) (R '[ k :-> v] .+ rest) .! k) ~ Vec n v
          )
       => Label k
       -> DataFrame n idx (r .+ rest)
       -> Series n idx v
series k df@DataFrame {..} = Series.construct $ Series.Options
  { optIndex = dfIndex
  , optData  = columnVec k df
  , optName  = Just (show k)
  }

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

toList :: ( Forall a Unconstrained1
          , SNatI n
          )
       => DataFrame n idx a
       -> [Rec a]
toList = Vec.toList . toVec

toNativeVec :: forall n idx t.
               ( Forall (NativeRow t) Unconstrained1
               , ToNative t
               , SNatI n
               )
            => DataFrame n idx (NativeRow t)
            -> Vec n t
toNativeVec = Vec.map Rec.toNative . toVec

toTexts :: forall n idx a.
           ( Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           , SNatI n
           )
        => DataFrame n idx a
        -> [[Text]]
toTexts df@DataFrame {-{..}-} {} = (headers:)
  . Vec.toList
  . Vec.map f
  $ vec
  where
    vec = toVec df

    headers :: [Text]
    headers = columns df

    f :: Rec a -> [Text]
    f = toFields headers

toVec :: ( Forall a Unconstrained1
         , SNatI n
         )
      => DataFrame n idx a
      -> Vec n (Rec a)
toVec DataFrame {..} = Rec.sequence dfData

valueCounts :: forall n idx r k v rest.
               ( r ≈ k .== v
               , Disjoint r rest
               , KnownSymbol k
               , Ord v
               , (Map (Vec n) (R '[ k :-> v] .+ rest) .! k) ~ Vec n v
               )
            => Label k
            -> DataFrame n idx (r .+ rest)
            -> Map.Map v Int
valueCounts k = colFoldr k (flip (Map.insertWith (+)) 1) mempty

colFoldr :: forall n idx r k v rest b.
            ( r ≈ k .== v
            , Disjoint r rest
            , KnownSymbol k
            , (Map (Vec n) ('R '[ k :-> v] .+ rest) .! k) ~ Vec n v
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

-- TODO I tried to do something like this:
--
--   case fromAnyDyn dyn of
--     Just (x :: forall a. ToField a => a) -> ...
--
-- but I couldn't get it working.  TODO: Ask on Haskell Slack.
fromAnyDyn :: Dynamic -> Maybe Text
fromAnyDyn dyn = case fromDynamic dyn of
  Just (t :: Text) -> Just t
  Nothing -> case fromDynamic dyn of
    Just (d :: Double) -> Just . show $ d
    Nothing -> case fromDynamic dyn of
      Just (i :: Integer) -> Just . show $ i
      Nothing -> case fromDynamic dyn of
        Just (i :: Int) -> Just . show $ i
        Nothing -> case fromDynamic dyn of
          Just (f :: Float) -> Just . show $ f
          Nothing -> case fromDynamic dyn of
            Just (s :: String) -> Just . cs $ s
            Nothing -> Nothing

getValue :: Text -> HashMap Text Dynamic -> Text
getValue k m = maybe noKeyError f $ HashMap.lookup k m
  where
    f v = fromAnyDyn v & fromMaybe noDynError

    noKeyError = "getValue failed: key not found in map: " <> k
    noDynError = "getValue failed: no value undynamicable"

-- TODO this obviously needs to be SOOO much better
lookup :: forall n idx a.
          ( Enum idx
          , Eq idx
          , Forall a Unconstrained1
          , LE n n
          , Num idx
          , SNatI n
          )
       => idx
       -> DataFrame n idx a
       -> Maybe (Rec a)
lookup k DataFrame {..} = rMay
  where
    rMay :: Maybe (Rec a)
    rMay = Vec.foldr f Nothing indexed
      where
        f :: (idx, Rec a) -> Maybe (Rec a) -> Maybe (Rec a)
        f (idx, r) Nothing
          | idx == k  = Just r
          | otherwise = Nothing
        f _        x       = x

    indexed :: Vec n (idx, Rec a)
    indexed = Index.index dfIndex (Rec.sequence dfData)

-- TODO move above to Vec.Extra.find? and then reimplement above as
--      instance of Vec.Extra.find

-- recDistributeL :: ( Forall r Unconstrained1
--                   , Foldable f
--                   , Functor f
--                   )
--                => f (Rec r)
--                -> ( Index m (Rec (Map f r))
--                   , Rec (Map f r)
--                   )
-- recDistributeL v = (idx, Rec.distribute v)
--   where
--     idx = panic "recDistributeL.idx"

-- recDistributeL :: ( Forall r Unconstrained1
--                   , Foldable f
--                   , Functor f
--                   )
--                => f (Rec r)
--                -> (Int, Rec (Map f r))
-- recDistributeL v = (len, Rec.distribute v)
--   where
--     len = length v

toFields :: ( Forall a ToField
            , Forall a Typeable
            )
         => [Text]
         -> Rec a
         -> [Text]
toFields headers r = List.map f headers
  where
    dm = Rec.toDynamicMap r

    f :: Text -> Text
    f k = getValue k dm


underL_ :: (Vec n (Rec a) -> Vec m (Rec b))
        -> Rec (Map (Vec n) a)
        -> ( Index m idx
           , Rec (Map (Vec m) b)
           )
underL_ _f _r = (idx, r')
  where
    idx = panic "underL_.idx"
    r'  = panic "underL_.r'"

-- underL_ :: forall n m a b.
--            ( Forall a Unconstrained1
--            , Forall b Unconstrained1
--            , SNatI n
--            , SNatI m
--            )
--         => (Vec n (Rec a) -> Vec m (Rec b))
--         -> Rec (Map (Vec n) a)
--         -> ( Index m (Rec b)
--            , Rec (Map (Vec m) b)
--            )
-- underL_ f = recDistributeL . f . Rec.sequence

-- underL_ :: forall n m a b.
--            ( Forall a Unconstrained1
--            , Forall b Unconstrained1
--            , SNatI n
--            , SNatI m
--            )
--         => (Vec n (Rec a) -> Vec m (Rec b))
--         -> Rec (Map (Vec n) a)
--         -> (Int, Rec (Map (Vec m) b))
-- underL_ f = _recDistributeL . f . Rec.sequence

under_ :: forall n m a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         , SNatI n
         , SNatI m
         )
      => (Vec n (Rec a) -> Vec m (Rec b))
      -> Rec (Map (Vec n) a)
      -> Rec (Map (Vec m) b)
under_ f = Rec.distribute . f . Rec.sequence

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
