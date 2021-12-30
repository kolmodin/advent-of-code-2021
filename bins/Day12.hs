{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (guard)
import Data.Char (isUpper, ord)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Input (readInputDay, splitBy)

newtype Cave = Cave Int deriving (Eq, Ord)

start, end :: Cave
start = Cave (-1)
end = Cave (-2)

isBig :: Cave -> Bool
isBig (Cave c) = c >= 0

toCave :: String -> Cave
toCave "start" = start
toCave "end" = end
toCave str@(x : _)
  | isUpper x = Cave (hash str)
  | otherwise = Cave (-(hash str))
toCave e = error ("toCave: unrecognized input \"" ++ e ++ "\"")

hash :: String -> Int
hash = foldl' (\acc c -> acc * 256 + ord c) 0

buildGraph :: [(Cave, Cave)] -> Map Cave [Cave]
buildGraph xs =
  Map.fromListWith (++) $ do
    (a, b) <- xs
    (from, to) <- [(a, b), (b, a)]
    guard (to /= start)
    return (from, [to])

parse :: String -> [(Cave, Cave)]
parse = map (extr . splitBy '-') . lines
  where
    extr [from, to] = (toCave from, toCave to)
    extr e = error ("parse: unknown parts: " ++ show e)

walkCount :: Bool -> Map Cave [Cave] -> Int
walkCount visitOnlyOnce cave = walk1 Set.empty visitOnlyOnce start
  where
    walkAll seen didTwice heres = sum (map (walk1 seen didTwice) heres)
    walk1 !seen !didTwice !here
      | here == end = 1
      | isBig here = walkAll seen didTwice fromHere
      | alreadyHere && didTwice = 0
      | alreadyHere = walkAll seen True fromHere
      | otherwise = walkAll (Set.insert here seen) (didTwice || alreadyHere) fromHere
      where
        alreadyHere = Set.member here seen
        fromHere = cave Map.! here

walk :: Bool -> Map Cave [Cave] -> [[Cave]]
walk visitOnlyOnce cave = go (Set.singleton start) visitOnlyOnce [start] start
  where
    go !_ _ path here
      | here == end = return path
    go seen twice path here = do
      to <- cave Map.! here
      let alreadySeen = Set.member to seen
      guard (not (alreadySeen && twice))
      let seen' = if isBig to || alreadySeen then seen else Set.insert to seen
       in go seen' (twice || alreadySeen) (to : path) to

-- Part 1: 4970
-- Part 2: 137948
main :: IO ()
main = do
  input <- parse <$> readInputDay 12
  let caves = buildGraph input
  -- putStrLn ("Part 1: " ++ show (length (walk True caves)))
  -- putStrLn ("Part 2: " ++ show (length (walk False caves)))

  putStrLn ("Part 1: " ++ show (walkCount True caves))
  putStrLn ("Part 2: " ++ show (walkCount False caves))
