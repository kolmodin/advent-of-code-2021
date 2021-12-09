{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Exception (assert)
import Coord (Coord (..), adjacent)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Char (digitToInt)
import qualified Data.Ix as Ix
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

toMap :: [[a]] -> Array Coord a
toMap lns =
  let lo = Coord 0 0
      hi = Coord (length lns - 1) (length (head lns) - 1)
   in assert
        ( let len = length (head lns)
           in all ((len==) . length) lns
        )
        $ Array.listArray (lo, hi) (concat lns)

lowPoints :: Array Coord Int -> [Int]
lowPoints arr =
  let bnds = Array.bounds arr
   in [ e
        | (i, e) <- Array.assocs arr,
          all (\i' -> arr Array.! i' > e) (filter (Ix.inRange bnds) (adjacent i))
      ]

riskPoints :: Array Coord Int -> [Int]
riskPoints = map (+ 1) . lowPoints

type Available = Set Coord

basins :: Available -> [Int]
basins available0
  | Set.null available0 = []
  | otherwise =
    let c = Set.findMin available0
        (n, available') = go available0 0 [c]
     in n : basins available'
  where
    go :: Available -> Int -> [Coord] -> (Int, Available)
    go available !n [] = (n, available)
    go available n (x : xs)
      | not (Set.member x available) = go available n xs
      | otherwise =
        go
          (Set.delete x available)
          (n + 1)
          (adjacent x ++ xs)

main :: IO ()
main = do
  world <- toMap . map (map digitToInt) . lines <$> readFile "day09.txt"
  putStrLn ("Part 1: " ++ show (sum (riskPoints world)))
  let available = Set.fromList [i | (i, n) <- Array.assocs world, n /= 9]
  putStrLn ("Part 2: " ++ show (product (take 3 (reverse (sort (basins available))))))
