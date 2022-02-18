{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
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

module Data.Frame.Typed
  ( AFrame(..)
  , Axes( Axes
        , columnLabels
        , rowLabels
        )
  , Frame
  , Verbosity(..)
  -- Constructors
  , cons
  , construct
  , empty
  , fromList
  , fromNativeVec
  , fromScalar
  , fromScalarList
  , fromSeries
  , fromVec
    -- Combinators
  , column
  , dropColumn
  , extend
  , extendFrom
  , extendWith
  , filterIndexes
  , head
  , map
  , overSeries
  , pop
  , rename
  , restrict
  , snoc
  , tail
  -- Optics
  , series
  -- Eliminators
  , a_
  , aframe
  , asBool
  , asScalar
  , asSeries
  , at
  , axes
  , bool
  , columnNames
  , columnVec
  , display
  , index
  , indexes
  -- , info
  , isEmpty
  -- , melt
  -- , meltSimple
  -- , memSize
  , ncols
  , ndims
  , nrows
  , onColumn
  , reify
  , render
  , shape
  , size
  , toFields
  , toList
  , toNativeVec
  , toTexts
  , toVec
  , valueCounts
  ) where

import           Data.Frame.Prelude         hiding ( bool
                                                   , empty
                                                   , foldr
                                                   , head
                                                   , map
                                                   , toList
                                                   )
import qualified Data.Frame.Prelude           as P

import qualified Data.List                    as List
import qualified Data.Map.Strict              as Map
import qualified Data.Row.Records             as Rec
import qualified Data.Text                    as Text
import qualified Data.Vec.Lazy.X              as Vec
import           Data.Frame.Typed.Index                  ( Index )
import qualified Data.Frame.Typed.Index       as Index
import           Data.Frame.Typed.Options                ( Options )
import qualified Data.Frame.Typed.Options     as Options
import qualified Data.Frame.Typed.Types.Table as Table
import           Data.Frame.Typed.Types.ToField          ( ToField )
import qualified Data.Frame.Typed.Utils.Dyn   as Dyn
import           Data.Frame.Typed.Series                 ( Series )
import qualified Data.Frame.Typed.Series      as Series

data Frame (n :: Nat) idx a = Frame
  { dfIndex :: Index n idx
  , dfData  :: Rec (Map (Series n idx) a)
  } deriving (Generic)

instance ( Forall (Map (Series n idx) a)  NFData
         , NFData idx
         )
  => NFData (Frame n idx a)

-- TODO: not the representaton we want but fine for Show... need
--       "real" functions  for rendering
deriving instance ( Forall (Map (Series n idx) a) Show
                  , Show idx
                  ) => Show (Frame n idx a)

deriving instance ( Forall (Map (Series n idx) a) Eq
                  , Eq idx
                  ) => Eq (Frame n idx a)

data AFrame idx a = forall n. SNatI n => AFrame
  { adfSize  :: SNat n
  , adfFrame :: Frame n idx a
  }

aframe :: forall n idx a.
          ( SNatI n )
       => Frame n idx a
       -> AFrame idx a
aframe = AFrame (snat @n)

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

-- https://pandas.pydata.org/docs/reference/api/pandas.Frame.html#pandas.Frame
construct :: forall n idx a.
             ( Forall a Unconstrained1
             , SNatI n
             , idx ~ Int
             )
          => Options n idx a
          -> Frame n idx a
construct opts = Frame dfIndex d
  where
    dfIndex = Options.optIndex opts

    d :: Forall a Unconstrained1 => Rec (Map (Series n idx) a)
    d = Rec.distribute source

    source :: Series n idx (Rec a)
    source = toSeries vecOfRecs

    vecOfRecs :: Vec n (Rec a)
    vecOfRecs = Options.optData opts

toLabeledSeries :: forall n idx a.
                   ( SNatI n
                   , idx ~ Int
                   )
                => Text
                -> Vec n a
                -> Series n idx a
toLabeledSeries label v = Series.construct $ Series.Options
  { optIndex = Index.defaultIntsFor v & fromMaybe (explode "Nahh")
  , optData  = v
  , optName  = Just label
  }

toSeries :: forall n idx a.
            ( SNatI n
            , idx ~ Int
            )
         => Vec n a
         -> Series n idx a
toSeries = toLabeledSeries "series"

empty :: Frame 'Z Int Empty
empty = construct . Options.fromVec $ Vec.empty

fromList :: forall n idx a.
            ( Forall a Unconstrained1
            , SNatI n
            , idx ~ Int
            )
         => [Rec a]
         -> Maybe (Frame n idx a)
fromList = fromVec <=< Vec.fromList

fromNativeVec :: forall n idx a t.
                 ( Forall a Unconstrained1
                 , Rec.FromNative t
                 , SNatI n
                 , a ~ Rec.NativeRow t
                 , idx ~ Int
                 )
              => Vec n t
              -> Frame n idx a
fromNativeVec values = Frame
  { dfData  = dfData'
  , dfIndex = Index.defaultIntsFor vecOfRecs `onCrash` "fromNativeVec.dfIndex"
  }
  where
    vecOfRecs :: Vec n (Rec (Rec.NativeRow t))
    vecOfRecs = Vec.map Rec.fromNative values

    recOfVecs :: Rec (Map (Vec n) (Rec.NativeRow t))
    recOfVecs = Rec.distribute vecOfRecs

    dfData' :: Rec (Map (Series n idx) (Rec.NativeRow t))
    dfData' = f' recOfVecs

    f' :: Rec (Map (Vec n) (Rec.NativeRow t))
       -> Rec (Map (Series n idx) (Rec.NativeRow t))
    f' = panic "fromNativeVec.something"

fromScalar :: forall idx a.
                  ( idx ~ Int )
               => a
               -> Maybe (Frame Nat1 idx ("value" .== a))
fromScalar x = fromVec (#value .== x ::: VNil)

fromScalarList :: forall n idx a.
                  ( SNatI n
                  , idx ~ Int
                  )
               => [a]
               -> Maybe (Frame n idx ("value" .== a))
fromScalarList = fromList . List.map (\x -> #value .== x)

fromSeries :: forall n idx a.
              ( SNatI n
              , idx ~ Int
              )
           => Series n idx a
           -> Maybe (Frame n idx ("value" .== a))
fromSeries = fromVec . Vec.map (#value .==) . Series.toVec

fromVec :: forall n idx a.
           ( Forall a Unconstrained1
           , SNatI n
           , idx ~ Int
           )
        => Vec n (Rec a)
        -> Maybe (Frame n idx a)
fromVec v = f <$> Index.defaultIntsFor v
  where
    f :: Index n idx -> Frame n idx a
    f idx = Frame
      { dfIndex = idx
      , dfData  = dfData'
      }
      where
        dfData' :: Rec (Map (Series n idx) a)
        dfData' = Rec.distribute seriesOfRecs

        seriesOfRecs :: Series n idx (Rec a)
        seriesOfRecs = Series.fromVec v & fromMaybe (explode "DF.fromVec")

-- ================================================================ --
--  Combinators
-- ================================================================ --

column :: forall n k v idx a b rest.
          ( Forall a Unconstrained1
          , Forall b Unconstrained1
          , SNatI n
          , Disjoint b rest
          , a ~ (b .+ rest)
          , b ≈ k .== v
          , idx ~ Int
          )
       => Label k
       -> Frame n idx a
       -> Frame n idx b
column _ = restrict

cons :: forall n idx a.
        ( Forall a Unconstrained1
        , SNatI n
        , idx ~ Int
        )
     => Rec a
     -> Frame n idx a
     -> Frame (Plus Nat1 n) idx a
cons x = overSeries (Series.cons x)

dropColumn :: forall k n idx a b r v.
              ( Disjoint r b
              , Forall a Unconstrained1
              , SNatI n
              , r ≈ k .== v
              , a ~ (r .+ b)
              , idx ~ Int
              )
           => Label k
           -> Frame n idx a
           -> Frame n idx b
dropColumn _ = restrict

extend :: forall n k v idx a b.
          ( Forall a Unconstrained1
          , Forall b Unconstrained1
          , KnownSymbol k
          , SNatI n
          , b ~ Extend k v a
          , idx ~ Int
          )
       => Label k
       -> v
       -> Frame n idx a
       -> Frame n idx b
extend k x = extendWith k (const x)

extendFrom :: forall k k' n v v' idx a b.
              ( Forall a Unconstrained1
              , Forall b Unconstrained1
              , KnownSymbol k
              , KnownSymbol k'
              , SNatI n
              , (a .! k) ~ v
              , b ~ Extend k' v' a
              , idx ~ Int
              )
           => Label k
           -> Label k'
           -> (v -> v')
           -> Frame n idx a
           -> Frame n idx b
extendFrom src dst f = extendWith dst (\r -> f (r .! src))

extendWith :: forall n k v idx a b.
              ( Forall a Unconstrained1
              , Forall b Unconstrained1
              , KnownSymbol k
              , SNatI n
              , b ~ Extend k v a
              , idx ~ Int
              )
           => Label k
           -> (Rec a -> v)
           -> Frame n idx a
           -> Frame n idx b
extendWith k f Frame {..} = Frame
  { dfIndex = dfIndex
  , dfData  = Rec.distribute seriesOfRecs
  }
  where
    seriesOfRecs :: Series n idx (Rec b)
    seriesOfRecs = fmap (Rec.extend k =<< f) . Rec.sequence $ dfData

head :: forall m n idx a.
        ( Forall a Unconstrained1
        , LE m n
        , SNatI m
        , SNatI n
        , idx ~ Int
        )
     => Frame n idx a
     -> Frame m idx a
head Frame {..} = Frame
  { dfIndex = Index.take dfIndex
  , dfData  = Rec.distribute seriesM
  }
  where
    seriesN = Rec.sequence dfData :: Series n idx (Rec a)
    seriesM = Series.take seriesN :: Series m idx (Rec a)

map :: forall n idx a b.
       ( Forall a Unconstrained1
       , Forall b Unconstrained1
       , SNatI n
       , idx ~ Int
       )
    => (Rec a -> Rec b)
    -> Frame n idx a
    -> Frame n idx b
map f = overSeries (fmap f)

-- melt :: forall m n idx a b.
--         Frame n idx a
--      -> Frame m idx b
-- melt = panic "melt"

-- Going to first try where you specify the full set of id cols and the full
-- set of melt cols in full k/v form.

-- If I can get that working then I'll try the same thing but where you just
-- pass the labels instead of the values.

-- Then I'll try making the id cols optional, then the melt cols optional.

-- Then both.

-- Then all of those in one functtion.

-- meltSimple :: Frame n idx a
--            -> Frame m idx b
-- meltSimple = panic "simpleMelt"

overSeries :: forall m n idx a b.
           ( Forall a Unconstrained1
           , Forall b Unconstrained1
           , SNatI m
           , SNatI n
           , idx ~ Int
           )
        => (Series n idx (Rec a) -> Series m idx (Rec b))
        -> Frame n idx a
        -> Frame m idx b
overSeries f Frame {..} = Frame
  { dfIndex = Index.defaultIntsFor srb `onCrash` "overSeries.dfIndex"
  , dfData  = Rec.distribute srb
  }
  where
    srb = (f . Rec.sequence) dfData :: Series m idx (Rec b)

pop :: forall n idx r k v rest.
       ( Disjoint r rest
       , Forall (r .+ rest) Unconstrained1
       , KnownSymbol k
       , SNatI n
       , Series n idx v ~ (Map (Series n idx) (r .+ rest) .! k)
       , r ≈ k .== v
       , idx ~ Int
       )
    => Label k
    -> Frame n idx (r .+ rest)
    -> (Frame n idx rest, Series n idx v)
pop k df = (restrict df, df ^. series k)

rename :: ( Extend k' (sa .! k) (sa .- k) ~ Map (Series n idx) b
          , KnownSymbol k
          , KnownSymbol k'
          , sa ~ Map (Series n idx) a
          , sb ~ Map (Series n idx) b
          )
       => Label k
       -> Label k'
       -> Frame n idx a
       -> Frame n idx b
rename k k' df = df { dfData = Rec.rename k k' (dfData df) }

restrict :: forall b n idx a.
            ( Forall a Unconstrained1
            , Forall b Unconstrained1
            , Rec.Subset b a
            , SNatI n
            , idx ~ Int
            )
         => Frame n idx a
         -> Frame n idx b
restrict = map Rec.restrict

snoc :: forall n idx a.
        ( Forall a Unconstrained1
        , SNatI n
        , idx ~ Int
        )
     => Rec a
     -> Frame n idx a
     -> Frame (Plus Nat1 n) idx a
snoc x = overSeries (Series.snoc x)

tail :: forall m n idx a.
        ( Forall a Unconstrained1
        , LE m n
        , SNatI n
        , SNatI m
        , idx ~ Int
        )
     => Frame n idx a
     -> Frame m idx a
tail Frame {..} = Frame
  { dfIndex = Index.drop dfIndex
  , dfData  = Rec.distribute seriesM
  }
  where
    seriesN = Rec.sequence dfData :: Series n idx (Rec a)
    seriesM = Series.drop seriesN :: Series m idx (Rec a)

-- ================================================================ --
--   Optics
-- ================================================================ --

series :: forall n idx r k v a rest.
          ( Disjoint r rest
          , (Map (Series n idx) a .! k) ~ Series n idx v
          , KnownSymbol k
          , r ≈ k .== v
          , a ~ (r .+ rest)
          )
       => Label k
       -> Lens' (Frame n idx a) (Series n idx v)
series k = lens get' set'
  where
    get' df   = dfData df .! k
    set' df s = df { dfData = Rec.update k s (dfData df) }

-- ================================================================ --
--   Eliminators
-- ================================================================ --

a_ :: forall idx a b.
      (forall n. SNatI n => Frame n idx a -> b)
   -> AFrame idx a
   -> b
a_ = reify

asBool :: forall idx k.
          ( KnownSymbol k
          , idx ~ Int
          )
       => Frame Nat1 idx (k .== Bool)
       -> Bool
asBool = asScalar

asScalar :: forall idx k v.
            ( KnownSymbol k
            , ToField v -- why??
            , idx ~ Int
            )
         => Frame Nat1 idx (k .== v)
         -> v
asScalar = Vec.head . Series.toVec . asSeries

bool :: forall idx a k.
        ( KnownSymbol k
        , idx ~ Int
        )
     => a
     -> a
     -> Frame Nat1 idx (k .== Bool)
     -> a
bool f t df = P.bool f t (asBool df)

asSeries :: forall n idx a k v.
          ( KnownSymbol k
          , SNatI n
          , ToField v
          , a ≈ k .== v
          , (a .! k) ~ v
          , ((k .== Vec n v) .! k) ~ Vec n ((k .== v) .! k)
          , idx ~ Int
          )
       => Frame n idx a
       -> Series n idx v
asSeries Frame {..} = Series.construct $ Series.Options
  { optIndex = dfIndex
  , optData  = optData'
  , optName  = Just $ Text.intercalate "-" (Rec.labels @a @ToField)
  }
  where
    optData' :: Vec n v
    optData' = fmap (snd . Rec.unSingleton)
      . Series.toVec
      . Rec.sequence
      $ dfData

at :: ( Forall a Unconstrained1
      , KnownSymbol k
      , LE n n
      , SNatI n
      , idx ~ Int
      )
   => idx
   -> Label k
   -> Frame n idx a
   -> Maybe (a .! k)
at idx k df = (.! k) <$> lookup idx df

-- TODO: Can't implement `iat` until we have some sense of column ordering

axes :: Forall a ToField
     => Frame n idx a
     -> Axes idx
axes df = Axes
  { rowLabels    = Vec.toList . Index.toVec . index $ df
  , columnLabels = columnNames df
  }

columnNames :: forall n idx a.
               ( Forall a ToField )
            => Frame n idx a
            -> [Text]
columnNames _ = Rec.labels @a @ToField

columnVec :: forall n idx a k v r rest.
             ( Disjoint r rest
             , KnownSymbol k
             , a ~ (r .+ rest)
             , v ~ (a .! k)
             , r ≈ k .== v
             , (Map (Series n idx) a .! k) ~ Series n idx v
             )
          => Label k
          -> Frame n idx a
          -> Vec n v
columnVec = flip onColumn identity

display :: forall n idx a.
           ( AllUniqueLabels (Map (Vec n) a)
           , Forall (Map (Vec n) a) Unconstrained1
           , Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           , Forall (Map (Vec n) a) Unconstrained1
           , SNatI n
           , idx ~ Int
           )
        => Frame n idx a
        -> IO ()
display = putStr
  . Table.render
  . orCrash "display explode"
  . Table.fromHeadedRows
  . List.map Table.Row
  . toTexts

index :: Frame n idx a -> Index n idx
index = dfIndex

-- | The index (row labels) of the Frame.
indexes :: forall n idx a.
           ( Enum idx
           , Eq idx
           , Forall a Unconstrained1
           , LE n n
           , Num idx
           , SNatI n
           , idx ~ Int
           )
        => Frame n idx a
        -> Vec n idx
indexes Frame {..} = Vec.map fst . Index.index dfIndex $ rows
  where
    rows :: Vec n (Rec a)
    rows = Series.toVec seriesOfRecs

    seriesOfRecs :: Series n idx (Rec a)
    seriesOfRecs = Rec.sequence dfData

isEmpty :: ( Enum idx
           , Forall a ToField
           )
        => Frame n idx a
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
--         => Frame idx a
--         -> m Word
-- memSize = (liftIO . Data.recursiveSize $!!)

ncols :: Forall a ToField
      => Frame n idx a
      -> Int
ncols = length . columnNames

-- I think this is right?
ndims :: Forall a ToField
      => Frame n idx a
      -> Int
ndims df
  | length (columnNames df) <= 1 = 1
  | otherwise                = 2

nrows :: Enum idx
      => Frame n idx a
      -> Int
nrows Frame {..} = Index.length dfIndex

onColumn :: forall n idx k v a b r rest.
            ( Disjoint r rest
            , KnownSymbol k
            , (Map (Series n idx) a .! k) ~ Series n idx v
            , a ~ (r .+ rest)
            , r ≈ k .== v
            )
         => Label k
         -> (Vec n v -> b)
         -> Frame n idx a
         -> b
onColumn k f = Series.onVec f . view (series k)

reify :: forall idx a b.
         (forall n. SNatI n => Frame n idx a -> b)
      -> AFrame idx a
      -> b
reify f (AFrame _n v) = f v

render :: forall n idx a.
          ( Forall a ToField
          , Forall a Typeable
          , Forall a Unconstrained1
          , SNatI n
          , idx ~ Int
          )
       => Frame n idx a
       -> Text
render df@Frame {..} = Table.render . Table.fromTexts $ headers:rows
  where
    headers = columnNames df

    rows = Vec.toList
      . Vec.map (toFields headers)
      . Series.toVec
      $ seriesOfRecs

    seriesOfRecs :: Series n idx (Rec a)
    seriesOfRecs = Rec.sequence dfData

shape :: ( Enum idx
         , Forall a ToField
         )
      => Frame n idx a
      -> (Int, Int)
shape = _onRC (,)

size :: ( Enum idx
        , Forall a ToField
        )
     => Frame n idx a
     -> Int
size = _onRC (*)

_onRC :: ( Enum idx
         , Forall a ToField
         )
      => (Int -> Int -> b)
      -> Frame n idx a
      -> b
_onRC f = f <$> nrows <*> ncols

toList :: forall n idx a.
          ( AllUniqueLabels (Map (Vec n) a)
          , Forall (Map (Vec n) a) Unconstrained1
          , Forall a Unconstrained1
          , SNatI n
          , idx ~ Int
          )
       => Frame n idx a
       -> [Rec a]
toList = Vec.toList . toVec

toNativeVec :: forall n idx t ntv.
               ( AllUniqueLabels (Map (Vec n) ntv)
               , Forall ntv Unconstrained1
               , Forall (Map (Vec n) ntv) Unconstrained1
               , ToNative t
               , SNatI n
               , idx ~ Int
               , ntv ~ NativeRow t
               )
            => Frame n idx ntv
            -> Vec n t
toNativeVec = Vec.map Rec.toNative . toVec

toTexts :: forall n idx a.
           ( AllUniqueLabels (Map (Vec n) a)
           , Forall (Map (Vec n) a) Unconstrained1
           , Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           , Forall (Map (Vec n) a) Unconstrained1
           , SNatI n
           , idx ~ Int
           )
        => Frame n idx a
        -> [[Text]]
toTexts df = addIndexes
  . (headers:)
  . Vec.toList
  . Vec.map f
  . Vec.zip (Index.toVec idx)
  . toVec
  $ df
  where
    headers :: [Text]
    headers = columnNames df

    idx = dfIndex df

    f :: (idx, Rec a) -> [Text]
    f (_n, r) = toFields headers r

    addIndexes :: [[Text]] -> [[Text]]
    addIndexes = zipWith (:) (mempty:fmap show (Index.toList idx))

toVec :: forall n idx a.
         ( AllUniqueLabels (Map (Vec n) a)
         , Forall (Map (Vec n) a) Unconstrained1
         , Forall a Unconstrained1
         , SNatI n
         , idx ~ Int
         )
      => Frame n idx a
      -> Vec n (Rec a)
toVec df = vecOfRecs
  where
    vecOfRecs :: Vec n (Rec a)
    vecOfRecs = Rec.sequence recOfVecs

    recOfSeries :: Rec (Map (Series n idx) a)
    recOfSeries = dfData df

    recOfVecs :: Rec (Map (Vec n) a)
    recOfVecs = recSeriesToRecVec recOfSeries

    recSeriesToRecVec :: Rec (Map (Series n idx) a)
                      -> Rec (Map (Vec n) a)
    recSeriesToRecVec ros = rov
      where
        sor :: Series n idx (Rec a)
        sor = Rec.sequence ros

        vor :: Vec n (Rec a)
        vor = Series.toVec sor

        rov :: Rec (Map (Vec n) a)
        rov = Rec.distribute vor

valueCounts :: forall n idx r k v rest.
               ( r ≈ k .== v
               , Disjoint r rest
               , KnownSymbol k
               , Ord v
               , (Map (Series n idx) (r .+ rest) .! k) ~ Series n idx v
               )
            => Label k
            -> Frame n idx (r .+ rest)
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
         -> Frame n idx (r .+ rest)
         -> b
colFoldr k f z df = Vec.foldr f z ( columnVec k df )

-- ================================================================ --
--  Helpers
-- ================================================================ --

-- TODO this obviously needs to be SOOO much better
lookup :: forall n idx a.
          ( Forall a Unconstrained1
          , LE n n
          , SNatI n
          , idx ~ Int
          )
       => Int
       -> Frame n idx a
       -> Maybe (Rec a)
lookup k Frame {..} = snd <$> find ((k ==) . fst) indexed
  where
    indexed :: Vec n (idx, Rec a)
    indexed = Index.index dfIndex vecOfRecs

    vecOfRecs :: Vec n (Rec a)
    vecOfRecs = Series.toVec . Rec.sequence $ dfData

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
             -> Frame n idx a
             -> Frame m idx a
filterIndexes f Frame {..} = Frame
  { dfIndex = Index.fromVec . f . Index.toVec $ dfIndex
  , dfData  = dfData'
  }
  where
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

-- ================================================================ --
--  TODOs
-- ================================================================ --

-- TODO: explain dtypes
-- TODO: explain select_dtypes
-- TODO: explain values replaced by toVec
-- TODO: explain to_numpy replaced by toVec

-- TODO: loc/locLabel

-- TODO: merge (however python does it)
--  ~ merge :: Frame idx a -> Frame idx b -> Extend a b

-- TODO group / groupBy
-- TODO append
-- TODO join
-- TODO melt

-- TODO use "Renderable" or something instead of Show for the idxs

-- info :: (Forall a ToField, Show idx)
--      => Verbosity
--      -> Frame n idx a
--      -> Text
-- info verbosity = case verbosity of
--   Quiet   -> infoQuiet
--   Verbose -> infoVerbose

-- infoQuiet :: (Forall a ToField, Show idx)
--           => Frame n idx a
--           -> Text
-- infoQuiet df = Text.unlines
--   [ showInternalRangeIndex (internalRangeIndex df)
--   , showColIndex (colIndex df)
--   ]

-- infoVerbose :: Show idx => Frame n idx a -> Text
-- infoVerbose df = Text.unlines
--   [ showInternalRangeIndex (internalRangeIndex df) <> "\nmore soon\n"
--   , "more soon (verbose)"
--   ]
