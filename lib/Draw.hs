{-# LANGUAGE FlexibleContexts #-}

module Draw where

import Coord (Coord (..), boundingBox)
import Data.Array.Unboxed (IArray, UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Map (Map)
import qualified Data.Map as Map

drawUArray :: IArray UArray e => (e -> Char) -> UArray Coord e -> String
drawUArray fmt arr =
  unlines
    [ [fmt (arr UArray.! Coord y x) | x <- [minx .. maxx]]
      | y <- [miny .. maxy]
    ]
  where
    (Coord miny minx, Coord maxy maxx) = UArray.bounds arr

drawMap :: Map Coord Char -> String
drawMap = drawMapWithBg ' '

drawMapWithBg :: Char -> Map Coord Char -> String
drawMapWithBg bg m =
  unlines
    [ [ Map.findWithDefault bg (Coord row col) m | col <- [minx .. maxx]]
      | row <- [miny .. maxy]
    ]
  where
    Just (Coord miny minx, Coord maxy maxx) = boundingBox (Map.keys m)
