{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Input (readInputDay)

parse :: String -> (Player, Player)
parse = toTuple . map (read . last . words) . lines
  where
    toTuple xs = let [a, b] = xs in (Player 0 a, Player 0 b)

type ID = Int

type Score = Int

type Pos = Int

type Counter = Int

data Player = Player  !Score  !Pos deriving (Eq, Ord, Show, Generic)

instance Hashable Player

winner :: Int -> Player -> Bool
winner winAt (Player score _) = score >= winAt

step :: Int -> Player -> Player
step n (Player score pos) =
  let pos' = ((pos + n - 1) `mod` 10) + 1
   in Player (score + pos') pos'

dieSequence :: [(Counter, Int)]
dieSequence = zip [1 ..] (cycle [1 .. 100])

play :: [(Counter, Int)] -> Player -> Player -> (Counter, Player)
play die pl1 pl2 =
  let dist = sum (map snd . take 3 $ die)
      rollCount = fst (die !! 2)
      die' = drop 3 die
      pl1' = step dist pl1
   in if winner 1000 pl1'
        then (rollCount, pl2)
        else play die' pl2 pl1'

type Universe = HashMap (Player, Player) Int

play2 :: Player -> Player -> [(ID, Counter)]
play2 pl10 pl20 = go 1 (HashMap.fromList [((pl10, pl20), 1)])
  where
    go :: ID -> Universe -> [(ID, Counter)]
    go pid univ0
      | HashMap.null univ0 = []
      | otherwise =
        let check pl1 pl2 cnt
              | winner 21 pl1 = Left cnt
              | otherwise = Right ((pl2, pl1), cnt)
            (sum -> !won, univ1) =
              partitionEithers
                [ check (step (roll1 + roll2 + roll3) pl1) pl2 cnt
                  | ((pl1, pl2), cnt) <- HashMap.toList univ0,
                    roll1 <- [1, 2, 3],
                    roll2 <- [1, 2, 3],
                    roll3 <- [1, 2, 3]
                ]
            score = (pid, won)
         in score : go (if pid == 1 then 2 else 1) (HashMap.fromListWith (+) univ1)

main :: IO ()
main = do
  (pl1, pl2) <- parse <$> readInputDay 21
  let (rolls, Player score _) = play dieSequence pl1 pl2
  putStrLn ("Part 1: " ++ show (rolls * score))
  let scores = play2 pl1 pl2
      maxScore = maximum $ Map.elems $ Map.fromListWith (+) scores
  putStrLn ("Part 2: " ++ show maxScore)