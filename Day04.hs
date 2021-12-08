{-# LANGUAGE TupleSections #-}

module Main (main) where

import Coord
import Data.Array (Array (), (//))
import qualified Data.Array as Array (assocs, elems, listArray, (!))
import Data.List (groupBy, partition)
import Input(cts)

split :: [String] -> [[String]]
split = filter (not . null . head) . groupBy (\x y -> null x == null y)

parse :: String -> [[Int]]
parse =
  map (map (read :: String -> Int) . words . unlines) . split . lines . cts

newtype Bingo = Bingo (Array Coord (Int, Bool))

instance Show Bingo where
  show (Bingo arr) =
    unlines
      [ unwords
          [ fmt (arr Array.! Coord row col) | col <- [0 .. 4]
          ]
        | row <- [0 .. 4]
      ]
    where
      fmt (i, b) = pad 3 (show i ++ if b then "*" else " ")
      pad n s = replicate (n - length s) ' ' ++ s

bingoCoords :: (Coord, Coord)
bingoCoords = (Coord 0 0, Coord 4 4)

toBingo :: [Int] -> Bingo
toBingo = Bingo . Array.listArray bingoCoords . map (,False)

mark :: Int -> Bingo -> Bingo
mark n0 (Bingo arr) =
  Bingo $
    arr // [(i, (n, True)) | (i, (n, _)) <- Array.assocs arr, n == n0]

winner :: Bingo -> Bool
winner (Bingo arr) =
  let wonRow row = and [snd $ arr Array.! Coord row col | col <- [0 .. 4]]
      wonCol col = and [snd $ arr Array.! Coord row col | row <- [0 .. 4]]
   in any wonRow [0 .. 4] || any wonCol [0 .. 4]

score :: Bingo -> Int
score (Bingo arr) = sum [n | (n, False) <- Array.elems arr]

run1 :: Int -> [Bingo] -> ([Bingo], [Bingo])
run1 draw bs0 =
  let bs = map (mark draw) bs0
   in partition winner bs

run :: [Int] -> [Bingo] -> [[Int]]
run [] _ = []
run (draw : xs) bs =
  let (winners, losers) = run1 draw bs
      rest = run xs losers
      winnerScores = map (\b -> draw * score b) winners
   in if null winnerScores
        then rest
        else winnerScores : rest

main :: IO ()
main = do
  (nums : boardNums) <- parse <$> readFile "day04.txt"
  let finalScores = run nums (map toBingo boardNums)
  print finalScores
  putStrLn ("Part 1: " ++ show (head finalScores))
  putStrLn ("Part 2: " ++ show (last finalScores))
