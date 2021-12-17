{-# LANGUAGE FlexibleContexts #-}

module Input where

import Control.Exception (assert)
import Coord (Coord (Coord))
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Char8 as C8
import System.Environment (getArgs)
import Text.Printf (printf)

readInputDay :: Int -> IO String
readInputDay day = C8.unpack <$> readInputDayC8 day

readInputDayC8 :: Int -> IO C8.ByteString
readInputDayC8 day = do
  args <- getArgs
  case args of
    [] -> C8.readFile (printf "inputs/day%02d.txt" day)
    ("-" : _) -> C8.getContents
    [file] -> C8.readFile file
    _ -> error ("readInputDay: unknown args: " ++ show args)

readInputDayArray :: IArray a Char => Int -> IO (a Coord Char)
readInputDayArray n = toArray . lines <$> readInputDay n

cts :: String -> String
cts = map (\c -> if c == ',' then ' ' else c)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c str0 = go str0
  where
    go str = case break (== c) str of
      (x, []) -> [x]
      (x, _ : xs) -> x : go xs

toArray :: IArray a e => [[e]] -> a Coord e
toArray lns =
  let rowlen = length (head lns)
      lo = Coord 0 0
      hi = Coord (length lns - 1) (rowlen - 1)
   in assert
        (all ((rowlen ==) . length) lns)
        (IArray.listArray (lo, hi) (concat lns))
