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
  , Options
  , ToField( toField )
  , Verbosity(..)
  , columns
  , fromList
  , fromScalarList
  , fromVector
  , index
  , info
  , map
  , memSize
  , renderWith
  ) where

import           Prelude               hiding   ( map )

-- import           Control.Lens                   ( Getter
--                                                 -- , Getting
--                                                 , view

--                                                 )
import           Control.DeepSeq                ( ($!!)
                                                , NFData
                                                )
import qualified Data.List            as List
import           Data.Row                       ( type (.==)
                                                , (.==)
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

opts :: (Enum idx, Num idx) => Options idx a
opts = Options
  { optIndexes = [0..]
  , optData    = Vector.empty
  }

construct :: Options idx a -> DataFrame idx a
construct Options {..} = DataFrame optIndexes optData
-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame

-- | The index (row labels) of the DataFrame.
index :: DataFrame idx a -> [idx]
index DataFrame {..} = List.take (Vector.length dfData) dfIndexes

columns :: forall idx a. (Forall a ToField) => DataFrame idx a -> [String]
columns _ = Rec.labels @a @ToField

data Verbosity
  = Quiet
  | Verbose
  deriving (Bounded, Enum, Generic, Eq, Ord, Show)

type RangeIndex idx = (Int, Maybe (idx, idx))

type ColIndex = (Int, Maybe (String, String))

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
memSize :: (Forall a NFData, NFData idx) => DataFrame idx a -> IO Word
memSize = (Data.recursiveSize $!!)

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

fromList :: (Enum idx, Num idx) => [Rec a] -> DataFrame idx a
fromList = fromVector . Vector.fromList

fromScalarList :: forall idx a. (Enum idx, Num idx)
               => [a]
               -> DataFrame idx ("value" .== a)
fromScalarList = fromList . List.map (\x -> #value .== x)

fromVector :: (Enum idx, Num idx) => Vector (Rec a) -> DataFrame idx a
fromVector recs = construct opts { optData = recs }

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
