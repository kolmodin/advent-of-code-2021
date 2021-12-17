{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Coord (Coord (Coord), adjacentBounded, manhattan)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Char (digitToInt)
import qualified Data.Ix as Ix
import Data.Maybe (fromMaybe)
import Input (readInputDay, toArray)
import Search (astarWith)

findLowestRisk :: UArray Coord Int -> Coord -> Coord -> Int
findLowestRisk cave from target =
  fromMaybe
    (error "target not in cave")
    (lookup target (astarWith from id next))
  where
    bnds = UArray.bounds cave
    next c =
      [ (manhattan c' target,
         cave UArray.! c', c')
        | c' <- adjacentBounded bnds c
      ]

makeLargeCave :: UArray Coord Int -> UArray Coord Int
makeLargeCave cave = UArray.listArray largeBnds [look c | c <- Ix.range largeBnds]
  where
    look (Coord y x) =
      let (tileY, y') = quotRem y lenY
          (tileX, x') = quotRem x lenX
          higher = manhattan (Coord 0 0) (Coord tileY tileX)
          c = cave UArray.! Coord y' x' + higher
       in if c <= 9 then c else c - 9
    (minc@(Coord miny minx), Coord maxy maxx) = UArray.bounds cave
    largeBnds = (minc, maxc)
    maxc = Coord (lenY * 5 - 1) (lenX * 5 - 1)
    lenY = maxy - miny + 1
    lenX = maxx - minx + 1

main :: IO ()
main = do
  cave <- toArray . map (map digitToInt) . lines <$> readInputDay 15
  putStrLn ("Part 1: " ++ show (uncurry (findLowestRisk cave) (UArray.bounds cave)))
  let cave' = makeLargeCave cave
  putStrLn ("Part 2: " ++ show (uncurry (findLowestRisk cave') (UArray.bounds cave')))
