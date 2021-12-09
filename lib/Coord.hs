module Coord where

import Data.Ix (Ix (inRange, range))
import GHC.Ix (unsafeIndex)

data Coord = Coord {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Ord, Eq, Show)

instance Ix Coord where
  range (Coord rowlow collow, Coord rowhi colhi) =
    [Coord row col | row <- [rowlow .. rowhi], col <- [collow .. colhi]]
  unsafeIndex (Coord rowlow collow, Coord _rowhi colhi) (Coord row col) =
    let fullRows = row - rowlow
        width = colhi - collow + 1
     in fullRows * width + col - collow
  inRange (Coord rowlow collow, Coord rowhi colhi) (Coord row col) =
    row >= rowlow && row <= rowhi && col >= collow && col <= colhi

bounds :: [Coord] -> Maybe (Coord, Coord)
bounds [] = Nothing
bounds (Coord row0 col0 : xs0) = Just (go ((row0, col0), (row0, col0)) xs0)
  where
    go ((row1, col1), (row2, col2)) [] = (Coord row1 col1, Coord row2 col2)
    go ((row1, col1), (row2, col2)) (Coord row col : xs) =
      go ((min row1 row, min col1 col), (max row2 row, max col2 col)) xs

adjacent :: Coord -> [Coord]
adjacent c = [up c, right c, down c, left c]

up :: Coord -> Coord
up (Coord row col) = Coord (row - 1) col

right :: Coord -> Coord
right (Coord row col) = Coord row (col + 1)

down :: Coord -> Coord
down (Coord row col) = Coord (row + 1) col

left :: Coord -> Coord
left (Coord row col) = Coord row (col - 1)