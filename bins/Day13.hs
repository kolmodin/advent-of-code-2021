{-# LANGUAGE TupleSections #-}

module Main (main) where

import Coord (Coord (Coord), coordFromXY, transposeCoord)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Draw (drawMap)
import Input (readInputDay, splitBy)

data Fold = X Int | Y Int deriving (Show)

parse :: String -> ([Coord], [Fold])
parse input =
  let [coordsLines, foldLines] = splitBy "" . lines $ input
   in (map pCoord coordsLines, map pFold foldLines)
  where
    pCoord = (\[x, y] -> coordFromXY (x, y)) . map read . splitBy ','
    pFold = pFoldParts . splitBy '='
    pFoldParts ["fold along x", x] = X (read x)
    pFoldParts ["fold along y", y] = Y (read y)
    pFoldParts e = error ("parse: " ++ show e)

foldPaper :: Fold -> [Coord] -> [Coord]
foldPaper (X x) = foldPaperAtX x
foldPaper (Y y) = map transposeCoord . foldPaperAtX y . map transposeCoord

foldPaperAtX :: Int -> [Coord] -> [Coord]
foldPaperAtX x = mapMaybe (foldAtX x)

foldAtX :: Int -> Coord -> Maybe Coord
foldAtX atX coord@(Coord y x)
  | x == atX = Nothing
  | x < atX = Just coord
  | otherwise = Just (Coord y (-x + 2 * atX))

main :: IO ()
main = do
  (coords, folds) <- parse <$> readInputDay 13
  let allPapers =
        map (Map.fromList . map (, '\x2588'))
          . tail
          . scanl (flip foldPaper) coords
          $ folds
  putStrLn ("Part 1: " ++ show (Map.size (head allPapers)))
  putStrLn "Part 2:"
  putStrLn (drawMap (last allPapers))
