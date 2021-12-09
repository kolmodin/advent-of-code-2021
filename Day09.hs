module Main (main) where

import Coord
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Char (digitToInt)
import qualified Data.Ix as Ix

toMap :: [[Int]] -> Array Coord Int
toMap lns =
  let lo = Coord 0 0
      hi = Coord (length lns - 1) (length (head lns) - 1)
   in Array.listArray (lo, hi) (concat lns)

lowPoints :: Array Coord Int -> [Int]
lowPoints arr =
  let bnds = Array.bounds arr
   in [ e
        | (i, e) <- Array.assocs arr,
          all (\i' -> arr Array.! i' > e) (filter (Ix.inRange bnds) (adjacent i))
      ]

riskPoints :: Array Coord Int -> [Int]
riskPoints = map (+ 1) . lowPoints

main :: IO ()
main = do
  txt <- map (map digitToInt) . lines <$> readFile "day09.txt"
  putStrLn ("Part 1: " ++ show (sum (riskPoints (toMap txt))))