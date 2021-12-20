module Main (main) where

import Coord (Coord (Coord), boundingBox, threeByThree)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Bits (setBit)
import Data.Foldable (foldl')
import qualified Data.Ix as Ix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Input (readInputDay)

type LUT = UArray Int Bool

data Image = Image !Bool !(Map Coord Bool)

parse :: String -> (LUT, Image)
parse str =
  let (lut : "" : img) = lines str
      lutArr = UArray.listArray (0, length lut - 1) (map c lut)
      c '.' = False
      c '#' = True
      c _ = error "c"
   in (lutArr, toImg (map (map c) img))

toImg :: [[Bool]] -> Image
toImg lns =
  Image False $
    Map.fromList
      [ (Coord rowIx colIx, True)
        | (rowIx, row) <- zip [0 ..] lns,
          (colIx, True) <- zip [0 ..] row
      ]

bitsToInt :: [Bool] -> Int
bitsToInt bits =
  foldl'
    (\acc (i, b) -> if b then setBit acc i else acc)
    0
    (zip [0 ..] (reverse bits))

lookupImg :: Coord -> Image -> Int
lookupImg c0 (Image def img) =
  bitsToInt $
    map (\c -> Map.findWithDefault def c img) (threeByThree c0)

enlarge :: (Coord, Coord) -> (Coord, Coord)
enlarge (Coord lorow locol, Coord hirow hicol) =
  (Coord (lorow - 1) (locol - 1), Coord (hirow + 1) (hicol + 1))

newPixel :: LUT -> Image -> Coord -> Bool
newPixel lut img c =
  let brightness = lookupImg c img
   in lut UArray.! brightness

enhance :: LUT -> Image -> Image
enhance lut img@(Image def pixels) =
  let bnds = enlarge (fromJust $ boundingBox (Map.keys pixels))
      def' = lut UArray.! (if def then 511 else 0)
      img' = Map.fromList [(c, newPixel lut img c) | c <- Ix.range bnds]
   in Image def' img'

pixelsLit :: Image -> Int
pixelsLit (Image False img) = length [() | True <- Map.elems img]
pixelsLit _ = error "pixelsLit: infinite"

main :: IO ()
main = do
  (lut, img0) <- parse <$> readInputDay 20
  let enhancements = iterate (enhance lut) img0
  print (pixelsLit (enhancements !! 2))
  print (pixelsLit (enhancements !! 50))