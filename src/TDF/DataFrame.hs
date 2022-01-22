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

module TDF.DataFrame
  ( Axes( Axes
        , columnLabels
        , rowLabels
        )
  , DataFrame
  , Options( optData
           , optIndexes
           )
  , Verbosity(..)
  , at
  , axes
  , column
  , columns
  , construct
  , display
  , empty
  , extend
  , extendWith
  , fromList
  , fromNativeVector
  , fromScalarList
  , fromVector
  , getColumn
  , head
  , head_
  , index
  , info
  , isEmpty
  , map
  , memSize
  , ndims
  , ncols
  , nrows
  , onColumn
  , onVec
  , opts
  , over
  , reindex
  , rename
  , render
  , restrict
  , shape
  , size
  , tail
  , tail_
  , toFields -- temporarily
  , toList
  , toNativeVector
  , toTexts
  , toVector
  , under
  ) where

import           TDF.Prelude              hiding ( empty
                                                 , head
                                                 , map
                                                 , toList
                                                 )

import           Control.DeepSeq                 ( ($!!)
                                                 , NFData
                                                 )
import           Data.Dynamic                    ( Dynamic
                                                 , fromDynamic
                                                 )
import           Data.HashMap.Strict             ( HashMap )
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.List            as List
import qualified Data.Row.Records     as Rec
import           Data.String                     ( String )
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector
import qualified GHC.DataSize         as Data
import qualified TDF.Types.Table      as Table
import           TDF.Types.ToField               ( ToField )
import           TDF.Types.RangeIndex            ( RangeIndex )
import qualified TDF.Types.SAI        as SA

data DataFrame idx a = DataFrame
  { dfIndex :: SA.Index idx
  , dfData  :: Rec (Map Vector a)
  } deriving (Generic)

instance (Forall (Map Vector a)  NFData, NFData idx)
  => NFData (DataFrame idx a)

-- TODO: not the representaton we want but fine for Show... need
--       "real" functions  for rendering
deriving instance ( Forall (Map Vector a) Show
                  , Show idx
                  ) => Show (DataFrame idx a)

deriving instance ( Forall (Map Vector a) Eq
                  , Eq idx
                  ) => Eq (DataFrame idx a)

data Options idx a = Options
  { optIndexes :: [idx]
  , optData    :: Vector (Rec a)
  }

data Axes idx = Axes
  { rowLabels    :: [idx]
  , columnLabels :: [Text]
  } deriving (Eq, Generic, Ord,  Show)

data Verbosity
  = Quiet
  | Verbose
  deriving (Bounded, Enum, Generic, Eq, Ord, Show)

opts :: Options Int a
opts = Options
  { optIndexes = [0..]
  , optData    = Vector.empty
  }

-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame
construct :: forall idx a.
             Forall a Unconstrained1
          => Options idx a
          -> DataFrame idx a
construct Options {..} = DataFrame dfIndexes d
  where
    dfIndexes :: [idx]
    dfIndexes = zipWith const optIndexes (Vector.toList optData)

    d :: Forall a Unconstrained1 => Rec (Map Vector a)
    d = Rec.distribute optData

-- | The index (row labels) of the DataFrame.
index :: DataFrame idx a -> [idx]
index DataFrame {..} = List.take n dfIndexes
  where
    n = panic "index: Vector.length dfData"

-- TODO: questionable to expose? at least with this interface... doing it for
--       now for Examples.hs
reindex :: [idx'] -> DataFrame idx a -> DataFrame idx' a
reindex dfIndexes' DataFrame {..} = DataFrame dfIndexes' dfData

columns :: forall idx a. (Forall a ToField) => DataFrame idx a -> [Text]
columns _ = Rec.labels @a @ToField

axes :: Forall a ToField => DataFrame idx a -> Axes idx
axes df@DataFrame {..} = Axes
  { rowLabels    = dfIndexes
  , columnLabels = columns df
  }

-- TODO: Can we do away with the `Forall a ToField` stuff by substituting
-- a contcreate ToField function from row-types?

ncols :: Forall a ToField => DataFrame idx a -> Int
ncols = length . columns

-- I think this is right?
ndims :: Forall a ToField => DataFrame idx a -> Int
ndims df
  | length (columns df) <= 1 = 1
  | otherwise                = 2

nrows :: DataFrame idx a -> Int
nrows = dfLength

dfLength :: DataFrame idx a -> Int
dfLength DataFrame {..} = List.length dfIndexes -- TODO better index types

-- TODO use "Renderable" or something instead of Show for the idxs

info :: (Forall a ToField, Show idx)
     => Verbosity
     -> DataFrame idx a
     -> Text
info verbosity = case verbosity of
  Quiet   -> infoQuiet
  Verbose -> infoVerbose

infoQuiet :: (Forall a ToField, Show idx)
          => DataFrame idx a
          -> Text
infoQuiet df = Text.unlines
  [ showInternalRangeIndex (internalRangeIndex df)
  , showColIndex (colIndex df)
  ]

infoVerbose :: Show idx => DataFrame idx a -> Text
infoVerbose df = Text.unlines
  [ showInternalRangeIndex (internalRangeIndex df) <> "\nmore soon\n"
  , "more soon (verbose)"
  ]

-- internalRangeIndex :: DataFrame idx a -> InternalRangeIndex idx
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

-- TODO truncate indexes on construction so this doesn't infintie loop
memSize :: ( Forall (Map Vector a) NFData
           , MonadIO m
           , NFData idx
           )
        => DataFrame idx a
        -> m Word
memSize = (liftIO . Data.recursiveSize $!!)

-- colIndex :: Forall a ToField => DataFrame idx a -> ColIndex
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

fromList :: Forall a Unconstrained1 => [Rec a] -> DataFrame Int a
fromList = fromVector . Vector.fromList

fromScalarList :: [a] -> DataFrame Int ("value" .== a)
fromScalarList = fromList . List.map (\x -> #value .== x)

fromVector :: Forall a Unconstrained1
           => Vector (Rec a)
           -> DataFrame Int a
fromVector recs = construct opts
  { optData    = recs
  , optIndexes = [0.. Vector.length recs]
  }

onVec :: forall idx a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vector (Rec a) -> Vector (Rec b))
      -> DataFrame idx a
      -> DataFrame idx b
onVec f DataFrame {..} = DataFrame
  { dfIndex = dfIndex'
  , dfData  = dfData'
  }
  where
    _ = dfData :: Rec (Map Vector a)
    (dfData', _dfLength') = ((,) <$> Rec.distribute <*> Vector.length)
                            . f
                            . Rec.sequence
                            $ dfData

    -- TODO update indexes instead of dfLength' above
    dfIndex' = dfIndexes

over :: forall idx a b.
        ( Forall a Unconstrained1
        , Forall b Unconstrained1
        )
     => (Rec a -> Rec b)
     -> DataFrame idx a
     -> DataFrame idx b
over f = under (Vector.map f)

recDistributeL :: ( Forall r Unconstrained1
                  , Foldable f
                  , Functor f
                  )
               => f (Rec r)
               -> (Int, Rec (Map f r))
recDistributeL v = (len, Rec.distribute v)
  where
    len = length v

underL_ :: forall a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vector (Rec a) -> Vector (Rec b))
      -> Rec (Map Vector a)
      -> (Int, Rec (Map Vector b))
underL_ f = recDistributeL . f . Rec.sequence

under_ :: forall a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vector (Rec a) -> Vector (Rec b))
      -> Rec (Map Vector a)
      -> Rec (Map Vector b)
under_ f = Rec.distribute . f . Rec.sequence

under :: forall idx a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vector (Rec a) -> Vector (Rec b))
      -> DataFrame idx a
      -> DataFrame idx b
under f DataFrame {..} = DataFrame
  { dfIndex = dfIndex'
  , dfData  = dfData'
  }
  where
    (_dfLength', dfData') = underL_ f dfData
    -- TODO: update indexes instead of dfLength
    dfIndex' = dfIndexes

map :: ( Forall a Unconstrained1
       , Forall b Unconstrained1
       )
    => (Rec a -> Rec b)
    -> DataFrame idx a
    -> DataFrame idx b
map f = onVec (Vector.map f)

column :: forall r idx k v rest.
          ( Disjoint r rest
          , Forall r Unconstrained1
          , Forall (r .+ rest) Unconstrained1
          , KnownSymbol k
          , r ≈ k .== v
          )
       => Label k
       -> DataFrame idx (r .+ rest)
       -> DataFrame idx r
column _ = map Rec.restrict

toTexts :: forall idx a.
           ( Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           )
        => DataFrame idx a
        -> [[Text]]
toTexts df@DataFrame {..} = (headers:)
  . Vector.toList
  . Vector.map f
  $ vec
  where
    vec = toVector df

    headers :: [Text]
    headers = columns df

    f :: Rec a -> [Text]
    f = toFields headers

render :: forall idx a.
          ( Forall a ToField
          , Forall a Typeable
          , Forall a Unconstrained1
          )
       => DataFrame idx a
       -> Text
render df@(DataFrame _idx rv) = Table.render . Table.fromTexts $ headers:rows
  where
    headers = columns df
    rows    = Vector.toList . Vector.map (toFields headers) $ vr

    vr :: Vector (Rec a)
    vr = Rec.sequence rv

toFields :: ( Forall a ToField
            , Forall a Typeable
            ) => [Text] -> Rec a -> [Text]
toFields headers r = List.map f headers
  where
    dm = Rec.toDynamicMap r

    f :: Text -> Text
    f k = getValue k dm

getValue :: Text -> HashMap Text Dynamic -> Text
getValue k m = maybe noKeyError f $ HashMap.lookup k m
  where
    f v = fromAnyDyn v & fromMaybe noDynError

    noKeyError = "getValue failed: key not found in map: " <> k
    noDynError = "getValue failed: no value undynamicable"

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

shape :: Forall a ToField => DataFrame idx a -> (Int, Int)
shape = (,) <$> nrows <*> ncols

size :: Forall a ToField => DataFrame idx a -> Int
size = (*) <$> ncols <*> nrows

toList :: Forall a Unconstrained1 => DataFrame idx a -> [Rec a]
toList = Vector.toList . toVector

toVector :: Forall a Unconstrained1 => DataFrame idx a -> Vector (Rec a)
toVector DataFrame {..} = Rec.sequence dfData

isEmpty :: Forall a ToField => DataFrame idx a -> Bool
isEmpty df
  | ncols df == 0 = True
  | nrows df == 0 = True
  | otherwise     = False

empty :: DataFrame () Empty
empty = construct $ Options
  { optIndexes = [] :: [()]
  , optData = Vector.empty
  }

head :: forall idx a.
        Forall a Unconstrained1
     => Int
     -> DataFrame idx a
     -> DataFrame idx a
head n df@DataFrame {..} = DataFrame
  { dfIndex = SA.take m dfIndex
  , dfData  = under_ f dfData
  }
  where
    f :: Forall a Unconstrained1 => Vector (Rec a) -> Vector (Rec a)
    f = Vector.take m

    m | n > 0     = n
      | otherwise = nrows df + n

head_ :: Forall a Unconstrained1 => DataFrame idx a -> DataFrame idx a
head_ = head 5

tail :: forall idx a.
        Forall a Unconstrained1
     => Int
     -> DataFrame idx a
     -> DataFrame idx a
tail n df@DataFrame {..} = DataFrame
  { dfIndexes = List.drop m dfIndexes
  , dfData    = under_ f dfData
  }
  where
    f :: Forall a Unconstrained1 => Vector (Rec a) -> Vector (Rec a)
    f = Vector.drop m

    m | n >= 0    = nrows df - n
      | otherwise = negate n

tail_ :: Forall a Unconstrained1 => DataFrame idx a -> DataFrame idx a
tail_ = tail 5

-- TODO we should really at least eliminate the `Forall a ToField` constraint
-- on things that are just getting the column count -- should be able to get
-- that without rendering them.
at :: ( Eq idx
      , Forall a Unconstrained1
      , KnownSymbol k
      )
   => idx
   -> Label k
   -> DataFrame idx a
   -> Maybe (a .! k)
at idx k df = (.! k) <$> lookup idx df

-- TODO this obviously needs to be SOOO much better
lookup :: (Eq idx, Forall a Unconstrained1)
       => idx
       -> DataFrame idx a
       -> Maybe (Rec a)
lookup k DataFrame {..} = fmap snd . Vector.find ((== k) . fst) $ indexed
  where
    dfData' = Rec.sequence dfData
    indexed = Vector.zip (Vector.fromList dfIndexes) dfData'

fromNativeVector :: forall t.
                    ( Rec.FromNative t
                    , Forall (Rec.NativeRow t) Unconstrained1
                    )
                 => Vector t
                 -> DataFrame Int (Rec.NativeRow t)
fromNativeVector values = DataFrame
  { dfData    = Rec.distribute recs
  , dfIndexes = [0 .. Vector.length recs]
  }
  where
    recs :: Vector (Rec (Rec.NativeRow t))
    recs = Vector.map Rec.fromNative values

toNativeVector :: forall idx t.
                  ( ToNative t
                  , Forall (NativeRow t) Unconstrained1
                  )
               => DataFrame idx (NativeRow t)
               -> Vector t
toNativeVector df@DataFrame {..} = Vector.map Rec.toNative . toVector $ df

-- TODO: iat -- maybe? or explain
-- TODO: explain bool
-- TODO: explain dtypes
-- TODO: explain select_dtypes
-- TODO: explain values replaced by toVector
-- TODO: explain to_numpy replaced by toVector

-- TODO
-- locLabel :: label -> DataFrame idx a
-- locLabel = undefined

-- project :: [forall lbl. Label lbl] -> DataFrame idx a -> DataFrame idx b
-- project = undefined

-- data ColumnSlice idx
--   = UpTo idx
--   | Through (idx, idx)
--   | Stepped (idx, idx, idx)
--   deriving (Eq, Generic, Ord, Show)

onColumn :: forall k idx a b.
            ( KnownSymbol k
            , (Map Vector a .! k) ~ Vector (a .! k)
            )
         => Label k
         -> (Vector (a Rec..! k) -> b)
         -> DataFrame idx a
         -> b
onColumn k f DataFrame {..} = f $ dfData .! k

getColumn :: forall k idx a.
             ( KnownSymbol k
             , (Map Vector a .! k) ~ Vector (a .! k)
             )
          => Label k
          -> DataFrame idx a
          -> Vector (a .! k)
getColumn = flip onColumn identity

restrict :: forall idx a b.
            ( Forall a Unconstrained1
            , Forall b Unconstrained1
            , Rec.Subset b a
            )
         => DataFrame idx a
         -> DataFrame idx b
restrict DataFrame {..}= DataFrame
  { dfIndexes = dfIndexes
  , dfData    = dfData'
  }
  where
    _ = dfData :: Rec (Map Vector a)

    vrec :: Forall a Unconstrained1 => Vector (Rec a)
    vrec = Rec.sequence dfData

    vrec' :: ( Forall a Unconstrained1
             , Forall b Unconstrained1
             )
          => Vector (Rec b)
    vrec' = Vector.map Rec.restrict vrec

    -- dfData' :: Rec (Map Vector b)
    dfData' :: ( Forall a Unconstrained1
               , Forall b Unconstrained1
               )
            => Rec  (Map Vector b)
    dfData' = Rec.distribute vrec'

rename :: forall k k' idx a b.
          ( Forall a Unconstrained1
          , Forall b Unconstrained1
          , KnownSymbol k
          , KnownSymbol k'
          , b ~ Rec.Extend k' (a .! k) (a .- k)
          )
       => Label k
       -> Label k'
       -> DataFrame idx a
       -> DataFrame idx b
rename k k' DataFrame {..} = DataFrame
  { dfIndexes = dfIndexes
  , dfData    = dfData'
  }
  where
    _ = dfData :: Rec (Map Vector a)

    dfDataI :: Forall a Unconstrained1 => Vector (Rec a)
    dfDataI = Rec.sequence dfData

    dfDataI' :: Forall a Unconstrained1 => Vector (Rec b)
    dfDataI' = Vector.map (Rec.rename k k') dfDataI

    dfData' :: Forall b Unconstrained1 => Rec (Map Vector b)
    dfData' = Rec.distribute dfDataI'

display :: ( Forall a ToField
           , Forall a Typeable
           , Forall a Unconstrained1
           )
        => DataFrame idx a
        -> IO ()
display = putStr
  . Table.render
  . fromMaybe explode
  . Table.fromHeadedRows
  . List.map Table.Row
  . toTexts
  where
    explode = panic "display explode"

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
--          -> DataFrame idx (a .+ rest)
--          -> DataFrame idx (b .+ rest)
-- swapCols k t k' df = rename t k'
--   . rename k' k
--   . rename k t
--   $ df

extend :: forall idx k v r.
          ( Forall r Unconstrained1
          , Forall (Rec.Extend k v r) Unconstrained1
          , KnownSymbol k
          )
       => Label k
       -> v
       -> DataFrame idx r
       -> DataFrame idx (Rec.Extend k v r)
extend k v = under $ Vector.map (Rec.extend k v)

extendWith :: forall idx k v r.
              ( Forall r Unconstrained1
              , Forall (Rec.Extend k v r) Unconstrained1
              , KnownSymbol k
              , Monoid v
              )
           => Label k
           -> (Rec r -> v)
           -> DataFrame idx r
           -> DataFrame idx (Rec.Extend k v r)
extendWith k f = under $ Vector.map (Rec.extend k =<< f)
