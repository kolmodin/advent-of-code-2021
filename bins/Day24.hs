{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Bits (Bits (unsafeShiftL), (.|.))
import Data.Char (intToDigit, isAlpha)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Input (readInputDay)

data Instr
  = Inp {-# UNPACK #-} !Var
  | Op !Op {-# UNPACK #-} !Var !VarLit
  deriving (Eq, Ord, Show)

data Op = Add | Mul | Div | Mod | Eql deriving (Eq, Ord, Show)

type Var = Char

data VarLit = Var {-# UNPACK #-} !Var | Lit {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

data Reg
  = Reg
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

hash :: Int -> Reg -> Int
hash len (Reg a b c d) =
  unsafeShiftL len 60
    .|. unsafeShiftL a 45
    .|. unsafeShiftL b 30
    .|. unsafeShiftL c 15
    .|. d

parse :: String -> [Instr]
parse = map (go . words) . lines
  where
    varorlit [var]
      | isAlpha var = Var var
    varorlit xs = Lit (read xs)
    go xs =
      case xs of
        ["inp", [var]] -> Inp var
        ["add", [var], vol] -> Op Add var (varorlit vol)
        ["mul", [var], vol] -> Op Mul var (varorlit vol)
        ["div", [var], vol] -> Op Div var (varorlit vol)
        ["mod", [var], vol] -> Op Mod var (varorlit vol)
        ["eql", [var], vol] -> Op Eql var (varorlit vol)
        _ -> error "parse"

get :: Reg -> Var -> Int
get (Reg x y z w) v =
  case v of
    'x' -> x
    'y' -> y
    'z' -> z
    'w' -> w
    _ -> error "get"

getVol :: Reg -> VarLit -> Int
getVol reg vol =
  case vol of
    Var v -> get reg v
    Lit v -> v

set :: Reg -> Var -> Int -> Reg
set (Reg x y z w) v i =
  case v of
    'x' -> Reg i y z w
    'y' -> Reg x i z w
    'z' -> Reg x y i w
    'w' -> Reg x y z i
    _ -> error "set"

data Run = NeedInp Reg [Int] (Int -> Run) | Done Reg [Int]

instance Show Run where
  show (NeedInp reg inps _) = "NeedInp " ++ show (reg, inps)
  show (Done reg inps) = "Done " ++ show (reg, inps)

eval :: Reg -> [Instr] -> Run
eval = go []
  where
    go inps reg [] = Done reg inps
    go inps reg (instr : instrs) =
      case instr of
        Inp v -> NeedInp reg inps $ \i ->
          let reg' = set reg v i
           in go (i : inps) reg' instrs
        Op Add v vol -> go1 v (+) vol
        Op Mul v vol -> go1 v (*) vol
        Op Div v vol -> go1 v div vol
        Op Mod v vol -> go1 v mod vol
        Op Eql v vol -> go1 v (\x y -> if x == y then 1 else 0) vol
      where
        go1 v f vol =
          let reg' = set reg v (get reg v `f` getVol reg vol)
           in go inps reg' instrs

isOk :: Reg -> Bool
isOk reg = get reg 'z' == 0

type Seen = IntSet

-- optimize :: [Instr] -> [Instr]
-- optimize = go []
--   where
--     go prev [] = reverse prev
--     go prev (x : xs) = go (pushUp x prev) xs

-- pushUp :: Instr -> [Instr] -> [Instr]
-- pushUp y [] = [y]
-- pushUp (Inp v) xs = Inp v : xs
-- pushUp y (x : xs)
--   | any (`elem` refs x) (refs y) = y : x : xs
--   | otherwise = x : pushUp y xs
--   where
--     refs (Inp v) = [v]
--     refs (Op _ v (Lit _)) = [v]
--     refs (Op _ v (Var w)) = [v, w]

run :: [Int] -> Run -> Either Seen (Int, Reg, String)
run try = go mempty
  where
    go seen0 = \case
      Done reg inp
        | isOk reg -> Right (IntSet.size seen0, reg, map intToDigit (reverse inp))
        | otherwise -> Left seen0
      NeedInp reg inp f
        | IntSet.member (hash (length inp) reg) seen0 -> Left seen0
        | otherwise ->
          let seen1 = IntSet.insert (hash (length inp) reg) seen0
              tryEach seen [] = Left seen
              tryEach seen (n : ns) =
                case go seen (f n) of
                  Left seen' -> tryEach seen' ns
                  Right res -> Right res
           in tryEach seen1 try

{-
unoptimized
real	8m17,354s
user	8m13,484s
sys	0m3,630s

bang on overything, Reg with unpacked fields:
real	4m3,744s
user	4m1,287s
sys	0m2,279s

IntSet with hash on Reg.
real	2m8,147s
user	2m6,976s
sys	0m1,112s
-}

main :: IO ()
main = do
  instrs <- parse <$> readInputDay 24
  mapM_ print instrs
  putStrLn ("Part 1: " ++ show (run [9, 8 .. 1] (eval (Reg 0 0 0 0) instrs)))
  putStrLn ("Part 2: " ++ show (run [1 .. 9] (eval (Reg 0 0 0 0) instrs)))