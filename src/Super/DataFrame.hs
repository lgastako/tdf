{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE GADTs                           #-}
{-# LANGUAGE KindSignatures                  #-}
{-# LANGUAGE OverloadedLabels                #-}
{-# LANGUAGE PartialTypeSignatures           #-}
{-# LANGUAGE StandaloneDeriving              #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Super.DataFrame
  ( DataFrame
  , column
  , columnWith
  , displayWith
  , drop
  , empty
  , foldr
  , fromList
  , map
  , onVec
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
import           Data.Vector                    ( Vector )
import qualified Data.Vector          as Vector
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

--import qualified SuperRecord
import GHC.TypeLits
-- import           GHC.OverloadedLabels           ( IsLabel )

newtype DataFrame a = DataFrame (Vector (Rec a))

deriving instance RecEq a a => Eq (DataFrame a)

-- construct :: Options -> DataFrame a
-- construct = undefined
-- -- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame

column :: ( KnownSymbol label
          , KnownNat (RecSize a)
          , KnownNat ((RecSize a - RecTyIdxH 0 label a) - 1)
          )
       => FldProxy label
       -> DataFrame a
       -> DataFrame _
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
           -> DataFrame a
           -> DataFrame _
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

fromList :: [Rec a] -> DataFrame a
fromList = DataFrame . Vector.fromList

drop :: Int -> DataFrame a -> DataFrame a
drop n (DataFrame v) = DataFrame (Vector.drop n v)

take :: Int -> DataFrame a -> DataFrame a
take n = onVec (Vector.take n)

onVec :: (Vector (Rec a) -> Vector (Rec b)) -> DataFrame a -> DataFrame b
onVec f (DataFrame v) = DataFrame (f v)

map :: (Rec a -> Rec b) -> DataFrame a -> DataFrame b
map f = onVec (Vector.map f)

foldr :: (Rec a -> b -> b) -> b -> DataFrame a -> b
foldr f z (DataFrame v) = Prelude.foldr f z v

displayWith :: (Rec a -> [String]) -> DataFrame a -> IO ()
displayWith f = putStr . renderWith f

renderWith :: (Rec a -> [String]) -> DataFrame a -> String
renderWith f (DataFrame v) = renderStrings headers rows
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

vector :: DataFrame a -> Vector (Rec a)
vector (DataFrame v) = v

empty :: DataFrame '[]
empty = DataFrame Vector.empty

toVector :: DataFrame a -> Vector (Rec a)
toVector (DataFrame v) = v

toList :: DataFrame a -> [Rec a]
toList = Vector.toList . toVector
