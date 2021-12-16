module Main (main) where

import Control.Monad.State as State
import Data.Bits (Bits (setBit, testBit))
import Data.Foldable (foldl')
import Input (readInputDay)
import Numeric (readHex)

type Version = Int

type TypeId = Int

data Packet = Packet Version TypeId Payload deriving (Show)

data Payload
  = Literal Int
  | Operator Op [Packet]
  deriving (Show)

data Op = Sum | Product | Min | Max | GreaterThan | LessThan | EqualTo
  deriving (Show)

hexToBits :: Char -> [Bool]
hexToBits h = bits
  where
    [(n, "")] = readHex [h]
    bits = [testBit (n :: Int) i | i <- [3, 2, 1, 0]]

toBits :: String -> [Bool]
toBits = concatMap hexToBits

type Parse a = State [Bool] a

getBit :: Parse Bool
getBit = head <$> getBits 1

getBits :: Int -> Parse [Bool]
getBits n = do
  bits <- State.get
  case splitAt n bits of
    (want, rest)
      | length want == n -> put rest >> return want
      | otherwise -> error ("getBits: wanted to take " ++ show n ++ " but only had " ++ show (length bits))

getInt :: Int -> Parse Int
getInt n = bitsToInt <$> getBits n

bitsToInt :: [Bool] -> Int
bitsToInt bits =
  foldl'
    (\acc (i, b) -> if b then setBit acc i else acc)
    0
    (zip [0 ..] (reverse bits))

pPacket :: Parse Packet
pPacket = do
  (ver, tid) <- (,) <$> getInt 3 <*> getInt 3
  case tid of
    0 -> pOperatorPacket ver tid Sum
    1 -> pOperatorPacket ver tid Product
    2 -> pOperatorPacket ver tid Min
    3 -> pOperatorPacket ver tid Max
    4 -> pLiteralPacket ver
    5 -> pOperatorPacket ver tid GreaterThan
    6 -> pOperatorPacket ver tid LessThan
    7 -> pOperatorPacket ver tid EqualTo
    _ -> error ("pPacket: unknown typeId: " ++ show tid)

pLiteralPacket :: Version -> Parse Packet
pLiteralPacket ver = do
  let payload = do
        b <- getBit
        if b
          then (++) <$> getBits 4 <*> payload
          else getBits 4
  Packet ver 4 . Literal . bitsToInt <$> payload

pOperatorPacket :: Version -> TypeId -> Op -> Parse Packet
pOperatorPacket ver tid op = do
  lenTypeId <- getBit
  sub <-
    if lenTypeId
      then do
        n <- getInt 11
        replicateM n pPacket
      else do
        n <- getInt 15
        isolate n (untilEOF pPacket)
  return (Packet ver tid (Operator op sub))

untilEOF :: Parse a -> Parse [a]
untilEOF p = do
  rest <- State.get
  if null rest
    then return []
    else (:) <$> p <*> untilEOF p

isolate :: Int -> Parse a -> Parse a
isolate n p = do
  bits <- getBits n
  let (x, left) = runParse ((,) <$> p <*> bitsLeft) bits
  if left == 0
    then return x
    else error ("isolate: parser requested " ++ show n ++ " bits but didn't consume all, left " ++ show left ++ " bits.")

runParse :: Parse a -> [Bool] -> a
runParse = State.evalState

endOfInput :: Parse Bool
endOfInput = gets (all not)

bitsLeft :: Parse Int
bitsLeft = State.gets length

versions :: Packet -> Int
versions (Packet ver _ payload) =
  ver + case payload of
    (Literal _) -> 0
    (Operator _ pkgs) -> sum (map versions pkgs)

eval :: Packet -> Int
eval (Packet _ _ payload) =
  case payload of
    Literal val -> val
    Operator op pkgs ->
      evalOperator op pkgs

evalOperator :: Op -> [Packet] -> Int
evalOperator op pkgs =
  let sub = map eval pkgs
      binOp f =
        let [x, y] = pkgs
         in if f (eval x) (eval y)
              then 1
              else 0
   in case op of
        Sum -> sum sub
        Product -> product sub
        Min -> minimum sub
        Max -> maximum sub
        GreaterThan -> binOp (>)
        LessThan -> binOp (<)
        EqualTo -> binOp (==)

main :: IO ()
main = do
  bits <- toBits <$> readInputDay 16
  let (packages, True) = runParse ((,) <$> pPacket <*> endOfInput) bits
  putStrLn ("Part 1: " ++ show (versions packages))
  putStrLn ("Part 2: " ++ show (eval packages))
