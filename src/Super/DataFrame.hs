{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE GADTs                           #-}
{-# LANGUAGE KindSignatures                  #-}
{-# LANGUAGE OverloadedLabels                #-}
{-# LANGUAGE PartialTypeSignatures           #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE StandaloneDeriving              #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Super.DataFrame
  ( DataFrame
  , Options
  , column
  , columns
  , columnWith
  , construct
  , displayWith
  , drop
  , empty
  , foldr
  , fromList
  , fromScalarList
  , fromVector
  , index
  , map
  , onVec
  , opts
  , relabel
  , relabel'
  , renderWith
  , take
  , toList
  , toVector
  , vector
  ) where

import qualified Prelude

import           Prelude                 hiding ( drop
                                                , foldr
                                                , map
                                                , take
                                                )
import           Data.List                      ( intercalate
                                                , transpose
                                                )
import qualified Data.List            as List
import           Data.Vector                    ( Vector )
import qualified Data.Vector          as Vector
import           GHC.TypeLits
import           SuperRecord                    ( Rec
                                                , RecEq
                                                , RecSize
                                                , RecTyIdxH
                                                , FldProxy
                                                , (&)
                                                , (:=)(..)
                                                , get
                                                , rnil
                                                )


data DataFrame idx a = DataFrame
  { dfIndexes :: [idx]
  , dfData    :: Vector (Rec a)
  }

deriving instance (RecEq a a, Eq idx) => Eq (DataFrame idx a)

data Options idx a = Options
  { optIndexes :: [idx]
  , optData    :: Vector (Rec a)
  }

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

columns :: DataFrame idx a -> [String]
columns = error "columns"

_names :: [label := value] -> [String]
_names = fmap name

name :: label := value -> String
name (label := _value) = symbolVal label

column :: ( KnownSymbol label
          , KnownNat (RecSize a)
          , KnownNat ((RecSize a - RecTyIdxH 0 label a) - 1)
          )
       => FldProxy label
       -> DataFrame idx a
       -> DataFrame idx _
--     -> DataFrame '[label := SuperRecord.RecTy label a]
column label = map (relabel label)

relabel :: ( KnownSymbol l
           , KnownNat (RecSize lts)
           , KnownNat ((RecSize lts - RecTyIdxH 0 l lts) - 1))
        => FldProxy l
        -> Rec lts
        -> Rec _
--      -> Rec '[l := SuperRecord.RecTy l lts]
relabel label x = label := (get label x) & rnil

columnWith :: ( KnownSymbol label
              , KnownNat (RecSize a)
              , KnownNat ((RecSize a - RecTyIdxH 0 label a) - 1)
              )
           => ( Rec _ -- '[label := SuperRecord.RecTy label a]
             -> Rec _
              )
           -> FldProxy label
           -> DataFrame idx a
           -> DataFrame idx _
--         -> DataFrame '[label := SuperRecord.RecTy label a]
columnWith g label = map (g <$> relabel' label)

relabel' :: ( KnownSymbol l
            , KnownNat (RecSize lts)
            , KnownNat ((RecSize lts - RecTyIdxH 0 l lts) - 1)
            )
         => FldProxy l
         -> Rec lts
         -> Rec _
--       -> Rec '[l := SuperRecord.RecTy l lts]
relabel' label x = label := get label x & rnil

fromList :: (Enum idx, Num idx) => [Rec a] -> DataFrame idx a
fromList recs = construct opts { optData = Vector.fromList recs }

-- TODO: change from 'value' to '0' ? maybe, maybe not... weird stuff.
fromScalarList :: forall idx a. (Enum idx, Num idx)
               => [a]
               -> DataFrame idx '["value" := a]
fromScalarList = fromList . List.map (\x -> #value := x & rnil)

fromVector :: (Enum idx, Num idx) => Vector (Rec a) -> DataFrame idx a
fromVector recs = construct opts { optData = recs }

drop :: Int -> DataFrame idx a -> DataFrame idx a
drop n (DataFrame idxs v) = DataFrame idxs' v'
  where
    idxs' = List.drop n idxs
    v'    = Vector.drop n v

take :: Int -> DataFrame idx a -> DataFrame idx a
take n = onVec (Vector.take n)

onVec :: (Vector (Rec a) -> Vector (Rec b))
      -> DataFrame idx a
      -> DataFrame idx b
onVec f (DataFrame idx v) = DataFrame idx (f v)

map :: (Rec a -> Rec b) -> DataFrame idx a -> DataFrame idx b
map f = onVec (Vector.map f)

foldr :: (Rec a -> b -> b) -> b -> DataFrame idx a -> b
foldr f z (DataFrame _idx v) = Prelude.foldr f z v

displayWith :: (Rec a -> [String]) -> DataFrame idx a -> IO ()
displayWith f = putStr . renderWith f

renderWith :: (Rec a -> [String]) -> DataFrame idx a -> String
renderWith f (DataFrame _idx v) = renderStrings headers rows
  where
    headers :: [String]
    headers = ["TODO", "fix", "column", "names"]

    rows :: Vector [String]
    rows = Vector.map f v

-- toKeys :: Rec a -> String
-- toKeys = undefined

-- TODO improve efficiency
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
      . intercalate " | "
      . zipWith pad maxWidths

    pad :: Int -> String -> String
    pad n s = replicate (length s - n) ' ' ++ s

    maxWidths :: [Int]
    maxWidths = id
      . fmap maximum
      . transpose
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

vector :: DataFrame idx a -> Vector (Rec a)
vector (DataFrame _idx v) = v

empty :: DataFrame idx '[]
empty = DataFrame [] Vector.empty

toVector :: DataFrame idx a -> Vector (Rec a)
toVector (DataFrame _idx v) = v

toList :: DataFrame idx a -> [Rec a]
toList = Vector.toList . toVector
