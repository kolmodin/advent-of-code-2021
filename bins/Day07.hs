{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Array (Array)
import qualified Data.Array as Array (listArray, (!))
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Char8 as C8
import Data.Ix (Ix)
import qualified Data.Ix as Ix
import qualified FlatParse.Basic as P
import Input (readInputDayC8)
import Search (binsearch)

parse :: C8.ByteString -> [Int]
parse str = case P.runParser p str of
  P.OK x _ -> x
  _ -> error "could not parse"
  where
    p = do
      num <- P.readInt
      nums <- P.many ($(P.char ',') *> P.readInt)
      return (num : nums)

type Pos = Int

type Fuel = Int

array' :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
array' bnds f = IArray.listArray bnds [f i | i <- Ix.range bnds]

align :: (Int -> Int -> Int) -> [Int] -> Fuel
align cost0 xs = costs IArray.! ix
  where
    Just ix = binsearch (lo, hi) (\i -> costs IArray.! i < costs IArray.! (i + 1))
    (lo, hi) = (minimum xs, maximum xs)
    cost = cost0 (hi - lo)
    costs = array' (lo, hi) (align1 cost xs) :: Array Pos Fuel

align1 :: (Int -> Int) -> [Int] -> Pos -> Fuel
align1 cost xs pos = sum [cost (abs (x - pos)) | x <- xs]

cost1 :: Int -> Int -> Int
cost1 _diff = id

cost2 :: Int -> Int -> Int
cost2 diff =
  let arr = Array.listArray (0, diff) cs
      cs = take (diff + 1) (scanl (+) 0 [1 ..])
   in (Array.!) arr

-- Part 1: 356179
-- Part 2: 99788435
main :: IO ()
main = do
  poss <- parse <$> readInputDayC8 7
  putStrLn ("Part 1: " ++ show (align cost1 poss))
  putStrLn ("Part 2: " ++ show (align cost2 poss))
