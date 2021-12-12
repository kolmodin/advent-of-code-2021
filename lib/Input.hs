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