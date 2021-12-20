{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (forM_, guard)
import Data.List (sort, transpose)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Input (cts, readInputDay, splitBy)

data Scanner = Scanner Int [XYZ] [[XYZ]] deriving (Show)

data Resolved = Resolved Int XYZ [XYZ] deriving (Show)

type XYZ = (Int, Int, Int)

parse :: String -> [Scanner]
parse = map pScanner . splitBy "" . lines

pScanner :: [String] -> Scanner
pScanner inp =
  let ["---", "scanner", n, "---"] = words (head inp)
      bs = sort $ map pXYZ (tail inp)
      rot3ds = map sort $ transpose $ map trans bs
   in Scanner (read n) bs rot3ds

pXYZ :: String -> XYZ
pXYZ ln =
  let [x, y, z] = words (cts ln)
   in (read x, read y, read z)

add :: XYZ -> XYZ -> XYZ
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

negateXYZ :: XYZ -> XYZ
negateXYZ (x, y, z) = (-x, -y, -z)

manhattanXYZ :: XYZ -> XYZ -> Int
manhattanXYZ (x1, y1, z1) (x2, y2, z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

sub :: XYZ -> XYZ -> XYZ
sub xyz1 xyz2 = add xyz1 (negateXYZ xyz2)

transform2 :: (Int, Int) -> [(Int, Int)]
transform2 (x, y) = [(x, y), (y, -x), (-x, -y), (-y, x)]

left :: XYZ -> XYZ
left (x, y, z) = (-z, y, x)

up :: XYZ -> XYZ
up (x, y, z) = (x, z, -y)

trans :: XYZ -> [XYZ]
trans (x, y, z) =
  let stick = [(x', y', z) | (x', y') <- transform2 (x, y)]
      around = take 4 $ iterate (map left) stick
      ups = map up stick
      downs = map (up . up . up) stick
   in concat $ ups : downs : around

resolveNow :: Scanner -> Resolved
resolveNow (Scanner n bs _) = Resolved n (0, 0, 0) bs

matchingPair :: Resolved -> Scanner -> Maybe Resolved
matchingPair (Resolved _ _ sc1) (Scanner b _ sc2s) = listToMaybe $ do
  -- For each pair in sc1 and sc2, see if assuming that they're on
  -- the same coordinate means that 12 of all the beacons
  -- overlap.
  -- There should be 12 overlapping beacons, we only need one to find the offset.
  -- Drop the redundant 11 to save some time, in case there is no match.
  b1 <- drop 11 sc1
  sc2 <- sc2s
  b2 <- sc2
  let pos2abs = b1 `sub` b2
      sc2' = map (add pos2abs) sc2
      found12 = findMatches 12 (length sc2) sc1 sc2'
  guard found12
  return (Resolved b pos2abs sc2')

findMatches ::
  Ord a =>
  -- | Number of matches we search.
  Int ->
  -- | Number of elements in second list.
  Int ->
  -- | First list.
  [a] ->
  -- | Second list.
  [a] ->
  Bool
findMatches 0 _ _ _ = True
findMatches _ _ [] _ = False
findMatches _ _ _ [] = False
findMatches matchesLeft n (x : xs) (y : ys)
  | matchesLeft > n = False
  | x == y = findMatches (matchesLeft - 1) (n - 1) xs ys
  | x < y = findMatches matchesLeft n xs (y : ys)
  | otherwise = findMatches matchesLeft (n - 1) (x : xs) ys

partitionMatch :: Resolved -> [Scanner] -> ([Resolved], [Scanner])
partitionMatch _ [] = ([], [])
partitionMatch sc1 (x : xs) =
  let (match, noMatch) = partitionMatch sc1 xs
   in case matchingPair sc1 x of
        Nothing -> (match, x : noMatch)
        Just x' -> (x' : match, noMatch)

resolve :: [Resolved] -> [Scanner] -> [Resolved]
resolve [] [] = []
resolve [] _ = error "resolve: unable to resolve all"
resolve (r : res) xxs =
  let (resolved, unmatched) = partitionMatch r xxs
   in r : resolve (resolved ++ res) unmatched

main :: IO ()
main = do
  (sc0 : scanners) <- parse <$> readInputDay 19
  let allResolved = resolve [resolveNow sc0] scanners
      xyzs = Set.fromList $ concat [xyzs0 | Resolved _ _ xyzs0 <- allResolved]
  forM_ allResolved $ \(Resolved n pos _) ->
    putStrLn ("Resolved " ++ show n ++ " at " ++ show pos)
  putStrLn ("Part 1: " ++ show (Set.size xyzs))
  let maxManhattan =
        maximum
          [ manhattanXYZ pos1 pos2
            | Resolved _ pos1 _ <- allResolved,
              Resolved _ pos2 _ <- allResolved
          ]
  putStrLn ("Part 2: " ++ show maxManhattan)