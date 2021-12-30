{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Coord (Coord (Coord), add, boundingBox)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.List (findIndex)
import Input (linesWithCoords, readInputDay)

type Cucumber = Char

type Sea = UArray Coord Char

east :: Coord
east = Coord 0 1

south :: Coord
south = Coord 1 0

dir :: Cucumber -> Coord
dir c = case c of
  '>' -> east
  'v' -> south
  _ -> error ("dir: " ++ show c)

parse :: String -> Sea
parse str = UArray.array bnds coordCucumber
  where
    Just bnds = boundingBox (map fst coordCucumber)
    coordCucumber = linesWithCoords (lines str)

step :: Coord -> Sea -> Sea
step bnds = step1 bnds 'v' . step1 bnds '>'

step1 :: Coord -> Cucumber -> Sea -> Sea
step1 bnds cucumber sea =
  UArray.accumArray
    (\ _ x -> x)
    '.'
    (UArray.bounds sea)
    [ new c cc
      | (c, cc) <- UArray.assocs sea,
        cc /= '.'
    ]
  where
    new c cc
      | cc /= cucumber = (c, cc)
      | otherwise =
        let c' = wrapAround bnds (c `add` dir cc)
         in if sea UArray.! c' == '.'
              then (c', cc)
              else (c, cc)

wrapAround :: Coord -> Coord -> Coord
wrapAround (Coord edgeY edgeX) (Coord y x) =
  let y' = y `mod` (edgeY + 1)
      x' = x `mod` (edgeX + 1)
   in Coord y' x'

main :: IO ()
main = do
  sea0 <- parse <$> readInputDay 25
  let (_, edge) = UArray.bounds sea0
  let seas = iterate (step edge) sea0
      space = zipWith (==) seas (tail seas)
  putStrLn ("Part 1: " ++ show (fmap (+ 1) (findIndex id space)))