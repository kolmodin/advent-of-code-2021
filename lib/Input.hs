module Input where

import Text.Printf(printf)

readInputDay :: Int -> IO String
readInputDay day = readFile (printf "inputs/day%02d.txt" day)

cts :: String -> String
cts = map (\c -> if c == ',' then ' ' else c)