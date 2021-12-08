module Main (main) where

import qualified Data.Array as Array (listArray, (!))
import Input (cts)

parse :: String -> [Int]
parse = map read . words . cts

type Pos = Int

type Fuel = Int

align :: (Int -> Int -> Int) -> [Int] -> Fuel
align cost0 xs = minimum brute
  where
    (lo, hi) = (minimum xs, maximum xs)
    cost = cost0 (hi - lo)
    brute = map (align1 cost xs) [lo..hi]

align1 :: (Int -> Int) -> [Int] -> Pos -> Fuel
align1 cost xs pos = sum [cost (abs (x - pos)) | x <- xs]

cost1 :: Int -> Int -> Int
cost1 _diff = id

cost2 :: Int -> Int -> Int
cost2 diff =
  let arr = Array.listArray (0, diff) cs
      cs = take (diff + 1) (scanl (+) 0 [1 ..])
   in (Array.!) arr

main :: IO ()
main = do
  poss <- parse <$> readFile "day07.txt"
  putStrLn ("Part 1: " ++ show (align cost1 poss))
  putStrLn ("Part 2: " ++ show (align cost2 poss))
