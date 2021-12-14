{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Input (readInputDay, splitBy)

parse :: String -> (String, [(String, Char)])
parse str =
  let [[template], rest] = splitBy "" . lines $ str
   in (template, map pPair rest)
  where
    pPair ln =
      let [pair, "->", [c]] = words ln
       in (pair, c)

score :: [(String, Int)] -> Int
score template =
  let 
      scores =
        map (`div` 2) . sort
          . Map.elems
          . Map.fromListWith (+)
          $ [(x, i) | (xs, i) <- template, x <- xs]
   in last scores - head scores

extractPairs :: String -> [(String, Int)]
extractPairs = Map.toList . Map.fromListWith (+) . map (,1) . splitPair
  where
    splitPair (x : y : xs) = [x, y] : splitPair (y : xs)
    splitPair _ = []

run :: Map String Char -> [(String, Int)] -> [(String, Int)]
run m pairs =
  Map.toList . Map.fromListWith (+) $ do
    (s@[a, b], i) <- pairs
    let c = m Map.! s
    [([a, c], i), ([c, b], i)]

main :: IO ()
main = do
  (template, pairs) <- parse <$> readInputDay 14
  let pairMap = Map.fromList pairs
  let allRuns = iterate (run pairMap) (extractPairs template)
      score' = score . (++) [([head template, last template], 1)]
  putStrLn ("Part 1: " ++ show (score' (allRuns !! 10)))
  putStrLn ("Part 2: " ++ show (score' (allRuns !! 40)))