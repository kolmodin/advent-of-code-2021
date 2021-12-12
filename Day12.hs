{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (guard)
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Input (readInputDay)

data Cave = Start | End | Small String | Big String deriving (Show, Ord, Eq)

toCave :: String -> Cave
toCave "start" = Start
toCave "end" = End
toCave str@(x : _)
  | isUpper x = Big str
  | otherwise = Small str
toCave e = error ("toCave: unrecognized input \"" ++ e ++ "\"")

buildGraph :: [(Cave, Cave)] -> Map Cave [Cave]
buildGraph xs =
  Map.fromListWith (++) $ do
    (a, b) <- xs
    (from, to) <- [(a, b), (b, a)]
    guard (to /= Start)
    return (from, [to])

parse :: String -> [(Cave, Cave)]
parse = map (extr . words . map dashToSpace) . lines
  where
    dashToSpace '-' = ' '
    dashToSpace c = c
    extr [from, to] = (toCave from, toCave to)
    extr e = error ("parse: unknown parts: " ++ show e)

walk :: Bool -> Map Cave [Cave] -> [[Cave]]
walk visitOnlyOnce cave = go (Set.singleton Start) visitOnlyOnce [Start] Start
  where
    go !_ _ path End = return path
    go seen twice path here = do
      to <- cave Map.! here
      let alreadySeen = Set.member to seen
      guard (not (alreadySeen && twice))
      let seen' = case to of
            Small _ -> Set.insert to seen
            _ -> seen
       in go seen' (twice || alreadySeen) (to : path) to

pretty :: [Cave] -> String
pretty = intercalate "," . map pretty1
  where
    pretty1 End = "end"
    pretty1 Start = "start"
    pretty1 (Big s) = s
    pretty1 (Small s) = s

main :: IO ()
main = do
  caves <- buildGraph . parse <$> readInputDay 12
  putStrLn ("Part 1: " ++ show (length (walk True caves)))
  putStrLn ("Part 2: " ++ show (length (walk False caves)))
