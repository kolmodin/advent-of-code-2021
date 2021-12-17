module Input where

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

cts :: String -> String
cts = map (\c -> if c == ',' then ' ' else c)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c str0 = go str0
  where
    go str = case break (== c) str of
      (x, []) -> [x]
      (x, _ : xs) -> x : go xs