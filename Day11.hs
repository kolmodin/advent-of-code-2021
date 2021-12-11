{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Coord (Coord (..), neighboursBounded)
import Data.Array.MArray (freeze, newArray, readArray, writeArray)
import qualified Data.Array.MArray as MArray
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Char (digitToInt, intToDigit)
import qualified Data.Ix as Ix
import Data.List (findIndex)
import Data.STRef (modifySTRef, newSTRef, readSTRef)

type Cave = UArray Coord Int

toCave :: [[Int]] -> Cave
toCave lns =
  let lo = Coord 0 0
      hi = Coord (length lns - 1) (length (head lns) - 1)
   in assert
        ( let len = length (head lns)
           in all ((len ==) . length) lns
        )
        (UArray.listArray (lo, hi) (concat lns))

tick :: Cave -> (Int, Cave)
tick arr0 = runST $ do
  let bnds = UArray.bounds arr0
  flashCount <- newSTRef 0
  hasFlashed <- newArray bnds False :: ST s (STUArray s Coord Bool)
  arr <- MArray.thaw arr0 :: ST s (STUArray s Coord Int)
  let mark c = do
        flashed <- readArray hasFlashed c
        unless flashed $ do
          n <- (\n -> (n + 1) `rem` 10) <$> readArray arr c
          writeArray arr c n
          when (n == 0) $ do
            writeArray hasFlashed c True
            modifySTRef flashCount (+ 1)
            mapM_ mark (neighboursBounded  bnds c)
  mapM_ mark (Ix.range bnds)
  (,) <$> readSTRef flashCount <*> freeze arr

pretty :: Cave -> String
pretty arr =
  unlines
    [ [intToDigit (arr UArray.! Coord row col) | col <- [0 .. 9]]
      | row <- [0 .. 9]
    ]

main :: IO ()
main = do
  cave <- toCave . map (map digitToInt) . lines <$> readFile "day11.txt"
  let allCaves = iterate (tick . snd) (0, cave)
      caves = take 100 (tail allCaves)
  putStrLn ("Part 1: " ++ show (sum (map fst caves)))
  putStrLn ("Part 2: " ++ show (findIndex ((== 100) . fst) allCaves))
