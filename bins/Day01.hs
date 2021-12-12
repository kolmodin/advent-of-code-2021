module Main (main) where

import Input(readInputDay)

countIncreases :: [Int] -> Int
countIncreases xs = length . filter (uncurry (<)) $ zip xs (tail xs)

sum3 :: [Int] -> [Int]
sum3 xs = zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)

main :: IO ()
main = do
  nums <- map read . lines <$> readInputDay 1
  putStrLn ("Part1: " ++ show (countIncreases nums))
  putStrLn ("Part2: " ++ show (countIncreases (sum3 nums)))
