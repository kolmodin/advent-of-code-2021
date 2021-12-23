{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Input (readInputDay, splitBy)

data Command = Command Bool Cube deriving (Eq, Ord, Show)

data Cube = Cube !Range !Range !Range deriving (Eq, Ord, Show)

data Range = Range !Int !Int deriving (Eq, Ord, Show)

type Reactor = [Cube]

parse :: String -> [Command]
parse = map pCommand . lines

pCommand :: String -> Command
pCommand str0 =
  let [onoroff, coords] = words str0
      ['x' : '=' : x, 'y' : '=' : y, 'z' : '=' : z] = splitBy ',' coords
      pCoord str =
        let [from, _, to] = splitBy '.' str
         in Range (read from) (read to)
   in Command
        (onoroff == "on")
        ( Cube
            (pCoord x)
            (pCoord y)
            (pCoord z)
        )

volume :: Cube -> Int
volume (Cube x y z) = dist x * dist y * dist z
  where
    dist (Range lo hi) = hi - lo + 1

sumReactor :: Reactor -> Int
sumReactor = sum . map volume

build :: [Command] -> Reactor
build = foldl go mempty
  where
    go reactor (Command isOn cube) =
      let maybeAdd
            | isOn = (cube :)
            | otherwise = id
       in maybeAdd $ concatMap (`go1` cube) reactor
    go1 pos neg
      | overlap3D pos neg = pos `cut` neg
      | otherwise = [pos]

-- | From a positive cube, cut out a negative cube,
-- returns list of positive Cubes.
cut :: Cube -> Cube -> [Cube]
cut pos@(Cube px py pz) neg@(Cube nx ny nz) =
  [ cube
    | x <- ranges px nx,
      y <- ranges py ny,
      z <- ranges pz nz,
      let cube = Cube x y z,
      overlap3D cube pos,
      not (overlap3D neg cube)
  ]
  where
    ranges (Range plo phi) (Range nlo nhi) =
      -- The ranges are both side inclusive [lo..hi].
      -- In 'ran', change to inclusive-exclusive [lo..hi).
      let ran = nub $ sort [plo, phi + 1, nlo, nhi + 1]
       in -- turn back into inclusive-inclusive [lo..hi]
          zipWith (\lo hi -> Range lo (hi - 1)) ran (tail ran)

overlap3D :: Cube -> Cube -> Bool
overlap3D
  (Cube ax ay az)
  (Cube bx by bz) =
    overlap1D ax bx && overlap1D ay by && overlap1D az bz

overlap1D :: Range -> Range -> Bool
overlap1D (Range alo ahi) (Range blo bhi)
  | ahi < blo = False
  | bhi < alo = False
  | otherwise = True

filter50 :: [Command] -> [Command]
filter50 = mapMaybe mkFit
  where
    isValidCube (Cube (Range xlo xhi) (Range ylo yhi) (Range zlo zhi)) =
      xlo <= xhi && ylo <= yhi && zlo <= zhi
    adjust (Range lo hi) = Range (max (-50) lo) (min 50 hi)
    mkFit (Command isOn (Cube x y z)) =
      let cube = Cube (adjust x) (adjust y) (adjust z)
       in if isValidCube cube then Just (Command isOn cube) else Nothing

main :: IO ()
main = do
  cmds <- parse <$> readInputDay 22
  let part1 = sumReactor (build (filter50 cmds))
      part2 = sumReactor (build cmds)
  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)
