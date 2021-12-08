module Main (main) where

import Control.Arrow (Arrow (first), (&&&))
import Data.List (group, sort, transpose)
import Data.Maybe (fromMaybe)

type Freq = (Int, Int)

freq :: [String] -> [Freq]
freq = map freq1 . transpose

freq1 :: String -> Freq
freq1 = extract . map (head &&& length) . group . sort
  where
    extract fq =
      let find c = fromMaybe 0 (lookup c fq)
       in (find '0', find '1')

gamma :: Freq -> Char
gamma (z, o) = if z > o then '0' else '1'

epsilon :: Freq -> Char
epsilon fq = if gamma fq == '0' then '1' else '0'

toDecimal :: String -> Int
toDecimal s =
  sum [digit d * 2 ^ e | (d, e) <- zip (reverse s) [0 ..]]
  where
    digit '0' = 0
    digit '1' = 1

run :: (Int -> Int -> Char) -> [String] -> String
run keep lns = go (zip lns lns)
  where
    go [] = error "run: empty"
    go [(s, orig)] = orig
    go lns =
      let (z, o) = freq1 [x | (x : _, _) <- lns]
       in go [(tail s, orig) | (s, orig) <- lns, head s == keep z o]

ox :: [String] -> String
ox = run (\o z -> if z >= o then '1' else '0')

co2 :: [String] -> String
co2 = run (\o z -> if o <= z then '0' else '1')

main :: IO ()
main = do
  lns <- lines <$> readFile "day03.txt"
  let fqs = freq lns
      eps = toDecimal (map gamma fqs)
      gam = toDecimal (map epsilon fqs)
  -- 1082324
  putStrLn ("Part 1: " ++ show (eps * gam))
  let oxRating = toDecimal (ox lns)
      co2Rating = toDecimal (co2 lns)
  -- 1353024
  putStrLn ("Part 2: " ++ show (oxRating * co2Rating))