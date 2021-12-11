{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Coord (Coord (..))
import qualified Coord
import qualified Data.Array.Unboxed as UArr
import Data.Ix (rangeSize)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Vector as Vector
import GHC.Arr (unsafeIndex)
import Input (cts, readInputDay)

type Pos = (Int, Int)

type Line = (Pos, Pos)

parse :: String -> (Pos, Pos)
parse s =
  let [x1, y1, "->", x2, y2] = words (cts s)
   in ((read x1, read y1), (read x2, read y2))

data Mode = SupportDiag | NoSupportDiag deriving (Eq)

draw :: Mode -> Line -> [Pos]
draw mode ln@(p1@(x1, y1), p2@(x2, y2))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | mode == NoSupportDiag = []
  | abs (x1 - x2) == abs (y1 - y2) =
    if x1 < x2
      then drawDiag p1 p2
      else drawDiag p2 p1
  | otherwise = error (show ln)

drawDiag :: Pos -> Pos -> [Pos]
drawDiag (x1, y1) (x2, y2) =
  zip [x1 .. x2] (step y1 y2)

step :: Int -> Int -> [Int]
step a b = [a, a + s .. b]
  where
    s
      | a < b = 1
      | otherwise = -1

boundingBox :: [Line] -> Maybe (Coord, Coord)
boundingBox = Coord.boundingBox . map toC . concatMap (\(p1, p2) -> [p1, p2])
  where
    toC (x, y) = Coord y x

-- 309 ms
countDangerMap :: Mode -> [Line] -> Int
countDangerMap mode lns =
  let mp = Map.fromListWith (+) [(p, 1::Int) | ln <- lns, p <- draw mode ln]
   in length [() | (_, n) <- Map.toList mp, n >= 2]

-- 21 ms
countDangerArr :: Mode -> [Line] -> Int
countDangerArr mode lns =
  let clns = [Coord row col | ln <- lns, (col, row) <- draw mode ln]
      arr :: UArr.UArray Coord Int
      arr = UArr.accumArray (+) 0 (fromJust (boundingBox lns)) (map (,1 :: Int) clns)
   in length [() | i <- UArr.elems arr, i >= 2]

-- 151 ms. Huh?
countDangerVector :: Mode -> [Line] -> Int
countDangerVector mode lns =
  let coords = [Coord row col | ln <- lns, (col, row) <- draw mode ln]
      !bnds = fromJust (boundingBox lns)
      !n = rangeSize bnds
      earr :: Vector.Vector Int
      earr = Vector.replicate n 0

      arr :: Vector.Vector Int
      arr = Vector.accum (+) earr [(unsafeIndex bnds coord, 1) | coord <- coords]
   in Vector.foldl' (\acc h -> if h >= 2 then acc + 1 else acc) 0 arr

main :: IO ()
main = do
  lns <- map parse . lines <$> readInputDay 5
  putStrLn ("Part 1: " ++ show (countDangerArr NoSupportDiag lns))
  putStrLn ("Part 1: " ++ show (countDangerArr SupportDiag lns))