{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Bits (Bits (unsafeShiftL), (.|.))
import Data.Char (intToDigit, isAlpha)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Input (readInputDay)

data Instr
  = Inp {-# UNPACK #-} !Var
  | OpLit !Op {-# UNPACK #-} !Var {-# UNPACK #-} !Lit
  | OpVar !Op {-# UNPACK #-} !Var {-# UNPACK #-} !Var
  deriving (Eq, Ord, Show)

data Op = Add | Mul | Div | Mod | Eql deriving (Eq, Ord, Show)

type Var = Char

type Lit = Int

data Reg
  = Reg
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

hash :: Int -> Reg -> Int
hash len (Reg 0 0 z 0) =
  unsafeShiftL len 60
    .|. z
hash len reg = error (show (len, reg))

parse :: String -> [Instr]
parse = map (go . words) . lines
  where
    varorlit op v [var]
      | isAlpha var = OpVar op v var
    varorlit op v lit = OpLit op v (read lit)
    go xs =
      case xs of
        ["inp", [var]] -> Inp var
        ["add", [var], vol] -> varorlit Add var vol
        ["mul", [var], vol] -> varorlit Mul var vol
        ["div", [var], vol] -> varorlit Div var vol
        ["mod", [var], vol] -> varorlit Mod var vol
        ["eql", [var], vol] -> varorlit Eql var vol
        _ -> error "parse"

get :: Reg -> Var -> Int
get (Reg x y z w) v =
  case v of
    'x' -> x
    'y' -> y
    'z' -> z
    'w' -> w
    _ -> error "get"

set :: Reg -> Var -> Int -> Reg
set (Reg x y z w) v i =
  case v of
    'x' -> Reg i y z w
    'y' -> Reg x i z w
    'z' -> Reg x y i w
    'w' -> Reg x y z i
    _ -> error "set"

data Run = NeedInp !Reg !(Int -> Run) | Done !Reg [Int]

instance Show Run where
  show (NeedInp reg _) = "NeedInp " ++ show reg
  show (Done reg inps) = "Done " ++ show (reg, inps)

eval :: Reg -> [Instr] -> Run
eval = go []
  where
    go inps reg [] = Done reg inps
    go inps reg (instr : instrs) =
      case instr of
        Inp v -> NeedInp (set reg v 0) $ \i ->
          let reg' = set reg v i
           in go (i : inps) reg' instrs
        OpLit Add v lit -> golit v (+) lit
        OpLit Mul v lit -> golit v (*) lit
        OpLit Div v lit -> golit v div lit
        OpLit Mod v lit -> golit v mod lit
        OpLit Eql v lit -> golit v (\x y -> if x == y then 1 else 0) lit
        OpVar Add v var -> govar v (+) var
        OpVar Mul v var -> govar v (*) var
        OpVar Div v var -> govar v div var
        OpVar Mod v var -> govar v mod var
        OpVar Eql v var -> govar v (\x y -> if x == y then 1 else 0) var
      where
        golit v f lit = go inps (set reg v (get reg v `f` lit)) instrs
        govar v f var = go inps (set reg v (get reg v `f` get reg var)) instrs

isOk :: Reg -> Bool
isOk reg = get reg 'z' == 0

optimize :: [Instr] -> [Instr]
optimize = go []
  where
    go prev [] = reverse prev
    go prev (x : xs) = go (pushUp x prev) xs

pushUp :: Instr -> [Instr] -> [Instr]
pushUp y@(OpLit Mul _ 0) (x : xs)
  | all (`notElem` refs x) (refs y) = x : pushUp y xs
  where
    refs (Inp v) = [v]
    refs (OpLit _ v _) = [v]
    refs (OpVar _ v w) = [v, w]
pushUp y xxs = y : xxs

type Seen = IntSet

run :: [Int] -> Run -> Either Seen (Int, Reg, String)
run try = go 0 mempty
  where
    go !len !seen0 = \case
      Done reg inp
        | isOk reg -> Right (IntSet.size seen0, reg, map intToDigit (reverse inp))
        | otherwise -> Left seen0
      NeedInp reg f
        | IntSet.member (hash len reg) seen0 -> Left seen0
        | otherwise ->
          let seen1 = IntSet.insert (hash len reg) seen0
              len' = len + 1
              tryEach seen [] = Left seen
              tryEach seen (n : ns) =
                case go len' seen (f n) of
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

Arrange so that x y w are set to 0, reducing the number of states.
real	1m20,473s
user	1m20,135s
sys	0m0,314s

Add bang on Reg in Run.
real	0m50,633s
user	0m50,466s
sys	0m0,097s

Specialize lit vs var.
real	0m44,374s
user	0m44,167s
sys	0m0,132s
-}

main :: IO ()
main = do
  instrs <- optimize . parse <$> readInputDay 24
  mapM_ print instrs
  putStrLn ("Part 1: " ++ show (run [9, 8 .. 1] (eval (Reg 0 0 0 0) instrs)))
  putStrLn ("Part 2: " ++ show (run [1 .. 9] (eval (Reg 0 0 0 0) instrs)))