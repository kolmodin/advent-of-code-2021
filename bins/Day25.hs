{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Coord (Coord (Coord), add, boundingBox)
import Data.List (findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Input (linesWithCoords, readInputDay)

data Cucumber = East | South deriving (Eq, Show)

type Sea = Map Coord Cucumber

east :: Coord
east = Coord 0 1

south :: Coord
south = Coord 1 0

dir :: Cucumber -> Coord
dir c = case c of
  East -> east
  South -> south

parse :: String -> Sea
parse = Map.fromList . mapMaybe cucumber . linesWithCoords . lines
  where
    cucumber (c, '>') = Just (c, East)
    cucumber (c, 'v') = Just (c, South)
    cucumber _ = Nothing

step :: Coord -> Sea -> Sea
step bnds = step1 bnds South . step1 bnds East

step1 :: Coord -> Cucumber -> Sea -> Sea
step1 bnds cucumber sea =
  Map.fromList
    [ (newCoord c cc, cc)
      | (c, cc) <- Map.toList sea
    ]
  where
    newCoord c cc =
      let c' = wrapAround bnds (c `add` dir cc)
       in if Map.notMember c' sea && cc == cucumber
            then c'
            else c

wrapAround :: Coord -> Coord -> Coord
wrapAround (Coord edgeY edgeX) (Coord y x) =
  let y' = y `mod` (edgeY + 1)
      x' = x `mod` (edgeX + 1)
   in Coord y' x'

main :: IO ()
main = do
  sea0 <- parse <$> readInputDay 25
  let Just (_, edge) = boundingBox (Map.keys sea0)
  let seas = iterate (step edge) sea0
      space = zipWith (==) seas (tail seas)
  putStrLn ("Part 1: " ++ show (fmap (+ 1) (findIndex id space)))