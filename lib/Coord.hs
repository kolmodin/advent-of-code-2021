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
