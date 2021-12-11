{-# LANGUAGE FlexibleContexts #-}

module Draw where

import Coord (Coord (..))
import Data.Array.Unboxed (IArray, UArray)
import qualified Data.Array.Unboxed as UArray

drawUArray :: IArray UArray e => (e -> Char) -> UArray Coord e -> String
drawUArray fmt arr =
  unlines
    [ [fmt (arr UArray.! Coord y x) | x <- [minx .. maxx]]
      | y <- [miny .. maxy]
    ]
  where
    (Coord miny minx, Coord maxy maxx) = UArray.bounds arr
