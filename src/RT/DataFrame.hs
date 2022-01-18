{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module RT.DataFrame
  ( DataFrame
  , Options( optData
           , optIndexes
           )
  , ToField( toField )
  , Verbosity(..)
  , at
  , columns
  , construct
  , empty
  , fromNativeVector
  , fromList
  , fromScalarList
  , fromVector
  , head
  , head_
  , index
  , info
  , isEmpty
  , map
  , memSize
  , ncols
  , ndims
  , nrows
  , opts
  , reindex
  , renderWith
  , shape
  , size
  , tail
  , tail_
  , toList
  , toNativeVector
  , toVector
  ) where

import           Prelude               hiding   ( head
                                                , lookup
                                                , map
                                                , tail
                                                )

import           Control.DeepSeq                ( ($!!)
                                                , NFData
                                                )
import           Control.Lens                   ( Getter
                                                , view
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.List            as List
import           Data.Row                       ( type (.==)
                                                , (.==)
                                                , Empty
                                                , Forall
                                                , Rec
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text            as T
import           Data.Vector                    ( Vector )
import qualified Data.Vector          as Vector
import           Data.Generics.Labels           ()
import qualified Data.Row.Records     as Rec
import           GHC.Generics                   ( Generic )
import qualified GHC.DataSize         as Data

data DataFrame idx a = DataFrame
  { dfIndexes :: [idx]
  , dfData    :: Vector (Rec a)
  } deriving (Generic)

instance (Forall a NFData, NFData idx) => NFData (DataFrame idx a)

deriving instance ( Forall a Show
                  , Show idx
                  ) => Show (DataFrame idx a)

deriving instance ( Forall a Eq
                  , Eq idx
                  ) => Eq (DataFrame idx a)

data Options idx a = Options
  { optIndexes :: [idx]
  , optData    :: Vector (Rec a)
  }

class ToField a where
  toField :: a -> Text

instance ToField Text where
  toField = id

instance ToField Integer where
  toField = T.pack . show

instance ToField Int where
  toField = T.pack . show

instance ToField String where
  toField = T.pack

data Verbosity
  = Quiet
  | Verbose
  deriving (Bounded, Enum, Generic, Eq, Ord, Show)

-- TODO find better name for this so as to not conflict with "real" RangeIndex
type RangeIndex idx = (Int, Maybe (idx, idx))
type ColIndex       = (Int, Maybe (String, String))

opts :: Options Int a
opts = Options
  { optIndexes = [0..]
  , optData    = Vector.empty
  }

-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame
construct :: Options idx a -> DataFrame idx a
construct Options {..} = DataFrame optIndexes optData

-- | The index (row labels) of the DataFrame.
index :: DataFrame idx a -> [idx]
index DataFrame {..} = List.take (Vector.length dfData) dfIndexes

-- TODO: questionable to expose? at least with this interface... doing it for
--       now for Examples.hs
reindex :: [idx'] -> DataFrame idx a -> DataFrame idx' a
reindex dfIndexes' DataFrame {..} = DataFrame dfIndexes' dfData

columns :: forall idx a. (Forall a ToField) => DataFrame idx a -> [String]
columns _ = Rec.labels @a @ToField

-- TODO: dtypes
-- TODO: select_dtypes


-- TODO: axes ?
-- data Axes
-- axes :: DataFrame idx a -> Axes
-- axes = error "axes not implemented"

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
nrows DataFrame {..} = Vector.length dfData

-- TODO use "Renderable" or something instead of Show for the idxs

info :: (Forall a ToField, Show idx)
     => Verbosity
     -> DataFrame idx a
     -> String
info verbosity = case verbosity of
  Quiet   -> infoQuiet
  Verbose -> infoVerbose

infoQuiet :: (Forall a ToField, Show idx)
          => DataFrame idx a
          -> String
infoQuiet df = unlines
  [ showRangeIndex (rangeIndex df)
  , showColIndex (colIndex df)
  ]

infoVerbose :: Show idx => DataFrame idx a -> String
infoVerbose df = unlines
  [ showRangeIndex (rangeIndex df) ++ "\nmore soon\n"
  , "more soon (verbose)"
  ]

-- TODO: this is poorly named, especially since we're probably about to implement
--       our own "real" RangeIndex type.
rangeIndex :: DataFrame idx a -> RangeIndex idx
rangeIndex df = ( length idxs
                , case idxs of
                    []    -> Nothing
                    (f:xs) -> case reverse xs of
                                [] -> Nothing
                                (l:_) -> Just (f, l)
                )
  where
    idxs = index df

showRangeIndex :: Show idx => RangeIndex idx -> String
showRangeIndex (n, Nothing)     = "Range index: " <> show n <> " entries."
showRangeIndex (n, Just (f, l)) = "Range index: " <> show n <> " entries, "
                               <> show f <> " to " <> show l

-- TODO truncate indexes on construction so this doesn't infintie loop
memSize :: (Forall a NFData, MonadIO m, NFData idx) => DataFrame idx a -> m Word
memSize = (liftIO . Data.recursiveSize $!!)

colIndex :: Forall a ToField => DataFrame idx a -> ColIndex
colIndex df = ( length cols
              , case cols of
                  []    -> Nothing
                  (f:xs) -> case reverse xs of
                              [] -> Nothing
                              (l:_) -> Just (f, l)
              )
  where
    cols = columns df

showColIndex :: ColIndex -> String
showColIndex (n, Nothing)     = "Columns: " <> show n <> " entries."
showColIndex (n, Just (f, l)) = "Columns: " <> show n <> " entries, "
                             <> show f <> " to " <> show l

fromList :: [Rec a] -> DataFrame Int a
fromList = fromVector . Vector.fromList

fromScalarList :: [a] -> DataFrame Int ("value" .== a)
fromScalarList = fromList . List.map (\x -> #value .== x)

fromVector :: Vector (Rec a) -> DataFrame Int a
fromVector recs = construct opts
  { optData    = recs
  , optIndexes = [0.. Vector.length recs]
  }

onVec :: (Vector (Rec a) -> Vector (Rec b))
      -> DataFrame idx a
      -> DataFrame idx b
onVec f (DataFrame idx v) = DataFrame idx (f v)

map :: (Rec a -> Rec b) -> DataFrame idx a -> DataFrame idx b
map f = onVec (Vector.map f)

-- column :: ( KnownSymbol label
--           , KnownNat (RecSize a)
--           , KnownNat ((RecSize a - RecTyIdxH 0 label a) - 1)
--           )
--        => FldProxy label
--        -> DataFrame idx a
--        -> DataFrame idx _
-- --     -> DataFrame '[label := SuperRecord.RecTy label a]
-- column label = map (relabel label)

-- -- relabel :: ( KnownSymbol l
-- --            , KnownNat (RecSize lts)
-- --            , KnownNat ((RecSize lts - RecTyIdxH 0 l lts) - 1))
-- --         => FldProxy l
-- --         -> Rec lts
-- --         -> Rec _
-- -- --      -> Rec '[l := SuperRecord.RecTy l lts]
-- -- relabel :: _ -> a ->
-- -- relabel :: Getter s a -> Rec _ -> Rec _
-- relabel :: forall s a. Getter s a
--         -> s
--         -> Rec ("value" .== a)
-- relabel label x = label' .== view label x
--   where
--     _value :: a
--     _value = view label x

--     label' :: Label "value"
--     label' = error "value"

renderWith :: (Rec a -> [String]) -> DataFrame idx a -> String
renderWith f (DataFrame _idx v) = renderStrings headers rows
  where
    headers :: [String]
    headers = ["TODO", "fix", "column", "names"]

    rows :: Vector [String]
    rows = Vector.map f v

renderStrings :: [String] -> Vector [String] -> String
renderStrings headers rows = unlines $
  [top, headerRow, middle] ++ renderedRows ++ [ bottom ]
  where
    renderedRows :: [String]
    renderedRows = Vector.toList $ Vector.map wrap rows

    headerRow :: String
    headerRow = wrap headers

    top, middle, bottom :: String
    top    = lineWith ('+', '-', 'v', '+')
    middle = lineWith ('+', '-', '+', '+')
    bottom = lineWith ('+', '-', '^', '+')

    wrap :: [String] -> String
    wrap = ("| " ++)
      . (++ " |")
      . List.intercalate " | "
      . zipWith pad maxWidths

    pad :: Int -> String -> String
    pad n s = replicate (length s - n) ' ' ++ s

    maxWidths :: [Int]
    maxWidths = fmap maximum
              . List.transpose
              . fmap (fmap length)
              $ allStrings

    lineWith :: (Char, Char, Char, Char) -> String
    lineWith (left, _mid, _break, _right) = left:rest
      where
        rest = "[REST]"

    allStrings :: [[String]]
    allStrings = headers:rowStrings

    rowStrings :: [[String]]
    rowStrings = Vector.toList rows

shape :: Forall a ToField => DataFrame idx a -> (Int, Int)
shape = (,) <$> nrows <*> ncols

size :: Forall a ToField => DataFrame idx a -> Int
size = (*) <$> ncols <*> nrows

toList :: DataFrame idx a -> [Rec a]
toList = Vector.toList . toVector

-- values replaced by toVector
-- to_numpy replaced by toVector
toVector :: DataFrame idx a -> Vector (Rec a)
toVector DataFrame {..} = dfData

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

head :: Int -> DataFrame idx a -> DataFrame idx a
head n df@DataFrame {..} = DataFrame
  { dfIndexes = List.take   m dfIndexes
  , dfData    = Vector.take m dfData
  }
  where
    m | n > 0     = n
      | otherwise = nrows df + n

head_ :: DataFrame idx a -> DataFrame idx a
head_ = head 5

tail :: Int -> DataFrame idx a -> DataFrame idx a
tail n df@DataFrame {..} = DataFrame
  { dfIndexes = List.drop   m dfIndexes
  , dfData    = Vector.drop m dfData
  }
  where
    m | n >= 0    = nrows df - n
      | otherwise = negate n

tail_ :: DataFrame idx a -> DataFrame idx a
tail_ = tail 5

-- TODO: bool

-- TODO we should really at least eliminate the `Forall a ToField` constraint
-- on things that are just getting the column count -- should be able to get
-- that without rendering them.
at :: Eq idx => idx -> Getter (Rec a) b -> DataFrame idx a -> Maybe b
at idx accessor df = view accessor <$> lookup idx df

-- TODO: iat -- maybe?

-- TODO this obviously needs to be SOOO much better
lookup :: Eq idx => idx -> DataFrame idx a -> Maybe (Rec a)
lookup k DataFrame {..} = fmap snd . Vector.find ((== k) . fst) $ indexed
  where
    indexed = Vector.zip (Vector.fromList dfIndexes) dfData

fromNativeVector :: Rec.FromNative t
                 => Vector t
                 -> DataFrame Int (Rec.NativeRow t)
fromNativeVector recs = DataFrame
  { dfData    = recs'
  , dfIndexes = [0 .. Vector.length recs']
  }
  where
    recs' = Vector.map Rec.fromNative recs

toNativeVector :: Rec.ToNative t => DataFrame idx (Rec.NativeRow t) -> Vector t
toNativeVector DataFrame {..} = Vector.map Rec.toNative dfData
