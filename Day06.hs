{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Input (cts, readInputDay)

parse :: String -> [Int]
parse = map read . words . cts

type Count = Int

build :: [Int] -> Map Int Count
build = Map.fromListWith (+) . map (,1)

tick :: Map Int Count -> Map Int Count
tick fish0 =
  let fish' = Map.mapKeys pred (Map.delete 0 fish0)
      new n = Map.fromList [(6, n), (8, n)]
   in Map.unionWith (+) fish' (new (Map.findWithDefault 0 0 fish0))

count :: Map Int Count -> Count
count = sum . Map.elems

main :: IO ()
main = do
  fish <- build . parse <$> readInputDay 6
  let fs = iterate tick fish
  putStrLn ("Part 1: " ++ show (count (fs !! 80)))
  putStrLn ("Part 2: " ++ show (count (fs !! 256)))