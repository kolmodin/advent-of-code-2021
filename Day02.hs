module Main (main) where

data Command = Forward Int | Depth Int

parse :: [String] -> Command
parse ["forward", n] = Forward (read n)
parse ["up", n] = Depth (-(read n))
parse ["down", n] = Depth (read n)

sim :: (Int, Int) -> Command -> (Int, Int)
sim (h, d) cmd =
  case cmd of
    Forward x -> (h + x, d)
    Depth x -> (h, d + x)

sim2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
sim2 (h, d, a) cmd =
  case cmd of
    Forward x -> (h + x, d + a * x, a)
    Depth x -> (h, d, a + x)

main :: IO ()
main = do
  cmds <- map (parse . words) . lines <$> readFile "day02.txt"
  do
    let (h, d) = foldl sim (0, 0) cmds
    putStrLn ("Part 1:" ++ show (h * d))
  do
    let (h, d, _) = foldl sim2 (0, 0, 0) cmds
    putStrLn ("Part 2:" ++ show (h * d))
