{-# LANGUAGE BangPatterns #-}

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

coordFromXY :: (Int, Int) -> Coord
coordFromXY (x, y) = Coord y x

transposeCoord :: Coord -> Coord
transposeCoord (Coord y x) = Coord x y

boundingBox :: [Coord] -> Maybe (Coord, Coord)
boundingBox [] = Nothing
boundingBox (Coord row0 col0 : xs0) = Just (go ((row0, col0), (row0, col0)) xs0)
  where
    go ((row1, col1), (row2, col2)) [] = (Coord row1 col1, Coord row2 col2)
    go ((row1, col1), (row2, col2)) (Coord row col : xs) =
      go ((min row1 row, min col1 col), (max row2 row, max col2 col)) xs

-- | 4 adjacent.
adjacent :: Coord -> [Coord]
adjacent !c = map ($ c) [up, right, down, left]

adjacentBounded :: (Coord, Coord) -> Coord -> [Coord]
adjacentBounded bnds = filter (inRange bnds) . adjacent

-- | 8 neigbours.
neighbours :: Coord -> [Coord]
neighbours !c =
  map
    ($ c)
    [ up,
      up . right,
      right,
      down . right,
      down,
      down . left,
      left,
      up . left
    ]

neighboursBounded :: (Coord, Coord) -> Coord -> [Coord]
neighboursBounded bnds = filter (inRange bnds) . neighbours

up :: Coord -> Coord
up (Coord row col) = Coord (row - 1) col

right :: Coord -> Coord
right (Coord row col) = Coord row (col + 1)

down :: Coord -> Coord
down (Coord row col) = Coord (row + 1) col

left :: Coord -> Coord
left (Coord row col) = Coord row (col - 1)