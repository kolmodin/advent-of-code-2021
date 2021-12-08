module Input where

cts :: String -> String
cts = map (\c -> if c == ',' then ' ' else c)