{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Control.Monad.State as State
import Data.Char (intToDigit)
import Data.List (partition, sort)
import Data.Maybe (fromJust)
import Input (readInputDay)

parse :: String -> ([String], [String])
parse ln =
  let (inp, "|" : out) = break (== "|") (words ln)
   in (map sort inp, map sort out)

count1478 :: ([String], [String]) -> Int
count1478 (_, out) = length . filter (`elem` [2, 3, 4, 7]) $ map length out

decodeInput :: [String] -> [(String, Int)]
decodeInput = State.evalState go
  where
    select name f = do
      nums <- State.get
      let (matches, rest) = partition f nums
      case matches of
        [x] -> do State.put rest; return x
        _ -> error ("decode.select: " ++ show (name, nums, matches, rest))
    selectLength name n = select name (\x -> length x == n)
    num1 `covers` num2 = all (`elem` num1) num2
    go = do
      one <- selectLength "one" 2
      four <- selectLength "four" 4
      seven <- selectLength "seven" 3
      eight <- selectLength "eight" 7
      three <- select "three" $ \three -> length three == 5 && three `covers` seven
      nine <- select "nine" $ \nine -> nine `covers` three
      zero <- select "zero" $ \zero -> zero `covers` seven
      six <- selectLength "six" 6
      five <- select "five" $ \five -> nine `covers` five
      two <- select "two" $ const True
      pure
        [ (zero, 0),
          (one, 1),
          (two, 2),
          (three, 3),
          (four, 4),
          (five, 5),
          (six, 6),
          (seven, 7),
          (eight, 8),
          (nine, 9)
        ]

decodeOutput :: [(String, Int)] -> [String] -> Int
decodeOutput dict xs =
  read
    [ intToDigit (fromJust (lookup x dict)) | x <- xs
    ]

decode :: ([String], [String]) -> Int
decode (inp, out) = decodeOutput (decodeInput inp) out

main :: IO ()
main = do
  input <- map parse . lines <$> readInputDay 8
  putStrLn ("Part 1: " ++ show (sum (map count1478 input)))
  putStrLn ("Part 2: " ++ show (sum (map decode input)))