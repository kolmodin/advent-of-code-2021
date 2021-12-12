module Input where

import System.Environment (getArgs)
import Text.Printf (printf)

readInputDay :: Int -> IO String
readInputDay day = do
  args <- getArgs
  case args of
    [] -> readFile (printf "inputs/day%02d.txt" day)
    ("-" : _) -> getContents
    [file] -> readFile file
    _ -> error ("readInputDay: unknown args: " ++ show args)

cts :: String -> String
cts = map (\c -> if c == ',' then ' ' else c)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c str0 = go str0
  where
    go str = case break (== c) str of
      (x, []) -> [x]
      (x, _ : xs) -> x : go xs