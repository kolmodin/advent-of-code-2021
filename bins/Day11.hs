{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Coord (Coord (..), neighboursBounded)
import Data.Array.MArray (freeze, newArray, readArray, writeArray)
import qualified Data.Array.MArray as MArray
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Char (digitToInt)
import qualified Data.Ix as Ix
import Data.List (findIndex)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Input (readInputDay, toArray)

type Cave = UArray Coord Int

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
            mapM_ mark (neighboursBounded bnds c)
  mapM_ mark (Ix.range bnds)
  (,) <$> readSTRef flashCount <*> freeze arr

main :: IO ()
main = do
  cave <- toArray . map (map digitToInt) . lines <$> readInputDay 11
  let allCaves = iterate (tick . snd) (0, cave)
      caves = take 100 (tail allCaves)
  putStrLn ("Part 1: " ++ show (sum (map fst caves)))
  putStrLn ("Part 2: " ++ show (findIndex ((== 100) . fst) allCaves))
