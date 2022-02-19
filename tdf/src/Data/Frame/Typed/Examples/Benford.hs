module Data.Frame.Typed.Examples.Benford where

import           Data.Char      ( digitToInt )
import qualified Data.Map  as M

digit :: Show a => a -> Int
digit = digitToInt . head . show

n :: Int
n = 1000

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

digits :: [Int]
digits = map digit . take n $ fibs

freqs :: M.Map Int Int
freqs = M.fromListWith (+) $ zip digits (repeat 1)

tab :: [(Int, Double, Double)]
tab =
  [ ( d
    , fromIntegral (M.findWithDefault 0 d freqs) / fromIntegral n
    , logBase 10.0 $ 1 + 1 / fromIntegral d
    )
  | d <- [1 .. 9]
  ]

main :: IO ()
main = mapM_ print tab
