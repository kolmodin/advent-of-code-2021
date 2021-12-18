module Main (main) where

import Data.Char (digitToInt, isDigit)
import Input (readInputDay)

data E = I Int | Open | Close deriving (Show)

type SF = [E]

parse :: String -> SF
parse [] = []
parse (x : xs) =
  case x of
    '[' -> Open : parse xs
    ']' -> Close : parse xs
    ',' -> parse xs
    _
      | isDigit x -> I (digitToInt x) : parse xs
      | otherwise -> error "parse"

addSF :: SF -> SF -> SF
addSF a b = reduceSF (Open : a ++ b ++ [Close])

reduceSF :: SF -> SF
reduceSF x =
  case explodeSF x of
    Nothing -> maybe x reduceSF (splitSF x)
    Just x' -> reduceSF x'

explodeSF :: SF -> Maybe SF
explodeSF = go [] (0 :: Int)
  where
    go _ 0 [] = Nothing
    go left depth (r : right) =
      case r of
        Open
          | depth == 4 -> -- oh boy, here we go!
            let ([I el, I er, Close], right') = splitAt 3 right
             in Just (reverse (collide left el) ++ I 0 : collide right' er)
          | otherwise ->
            go (r : left) (depth + 1) right
        Close -> go (r : left) (depth - 1) right
        I _ -> go (r : left) depth right
    go _ _ _ = error "explodeSF: no more input but not at depth 0"

collide :: SF -> Int -> SF
collide [] _ = []
collide (x : xs) n = case x of
  I n0 -> I (n + n0) : xs
  _ -> x : collide xs n

splitSF :: SF -> Maybe SF
splitSF = go []
  where
    go _ [] = Nothing
    go left (x : right) =
      case x of
        I n
          | n >= 10 ->
            let (el, er) = (n `div` 2, (n + 1) `div` 2)
             in Just (reverse left ++ [Open, I el, I er, Close] ++ right)
        _ -> go (x : left) right

magnitudeSF :: SF -> Int
magnitudeSF = fst . go
  where
    go [] = (0, [])
    go (x : xs) =
      case x of
        I n -> (n, xs)
        Open ->
          let (el, xs1) = go xs
              (er, xs2) = go xs1
              (Close : xs3) = xs2
           in (3 * el + 2 * er, xs3)
        Close -> error "magnitudeSF: close"

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

main :: IO ()
main = do
  lns <- map parse . lines <$> readInputDay 18
  let sumSF = foldl1 addSF lns
  putStrLn ("Part 1: " ++ show (magnitudeSF sumSF))
  putStrLn
    ( "Part 2: "
        ++ show
          ( maximum
              [ magnitudeSF (addSF a b)
                | (a, b) <- pairs lns
              ]
          )
    )
