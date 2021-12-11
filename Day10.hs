module Main (main) where

import Data.List (sort)
import Input (readInputDay)

isOpen :: Char -> Bool
isOpen c = c `elem` "<({["

close :: Char -> Char
close c = case c of
  '<' -> '>'
  '(' -> ')'
  '{' -> '}'
  '[' -> ']'
  _ -> error ("close: tried to close '" ++ [c] ++ "'")

score :: Char -> Int
score c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> error ("score: tried to score '" ++ [c] ++ "'")

data ErrorOrComplete = SyntaxError Char | Incomplete String

errorOrComplete :: String -> ErrorOrComplete
errorOrComplete = go []
  where
    go trail [] = Incomplete trail
    go trail (x : xs)
      | isOpen x = go (close x : trail) xs
      | otherwise = case trail of
        [] -> error "errorOrComplete: close but stack empty"
        (c : cs)
          | x == c -> go cs xs
          | otherwise -> SyntaxError x

score2 :: String -> Int
score2 = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (acc * 5 + scoreC x) xs
    scoreC c = case c of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      _ -> error ("score2: tried to score '" ++ [c] ++ "'")

-- Part 1: 294195
-- Part 2: 3490802734
main :: IO ()
main = do
  lns <- lines <$> readInputDay 10
  let result = map errorOrComplete lns
  putStrLn ("Part 1: " ++ show (sum [score c | SyntaxError c <- result]))
  let scores = sort [score2 ac | Incomplete ac <- result]
      mid = length scores `div` 2
  putStrLn ("Part 2: " ++ show (scores !! mid))