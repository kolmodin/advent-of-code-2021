{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Coord (Coord (Coord), adjacent, boundingBox, manhattan)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Char (isAlpha, isUpper)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Input (linesWithCoords, readInputDay)
import Search (astarWith, dfs)

data Amphipod = A | B | C | D deriving (Eq, Ord, Show)

type StepsLeft = Int

newtype State = State (Map Coord (Amphipod, StepsLeft)) deriving (Eq, Ord, Show)

amphipodCost :: Amphipod -> Int
amphipodCost a =
  case a of
    A -> 1
    B -> 10
    C -> 100
    D -> 1000

lookupSideRoom :: Amphipod -> Int
lookupSideRoom a =
  case a of
    A -> 3
    B -> 5
    C -> 7
    D -> 9

hallwayCoords :: [Coord]
hallwayCoords = [Coord 1 x | x <- [1, 2, 4, 6, 8, 10, 11]]

type SideRooms = (Int, Int)

type BurrowArr = UArray Coord Char

sideRoomCoords :: Show a => SideRooms -> Amphipod -> Map Coord (Amphipod, a) -> Maybe Coord
sideRoomCoords (ylo, yhi) = go
  where
    go a others =
      let othersInMyRoom = any otherInMyRoom1 (Map.toList others)
          otherInMyRoom1 (Coord _ x, (other, _)) = other /= a && lookupSideRoom a == x
       in if othersInMyRoom
            then Nothing
            else
              listToMaybe
                [ coord
                  | y <- [yhi, yhi - 1 .. ylo],
                    let coord = Coord y (lookupSideRoom a),
                    Map.notMember coord others
                ]

initState :: [(Coord, Amphipod)] -> State
initState amphipods = State (Map.fromList [(c, (a, 2)) | (c, a) <- amphipods])

heuristic :: State -> Int
heuristic (State pods) = sum . map (uncurry heuristic1) . Map.toList $ pods
  where
    heuristic1 coord@(Coord _ x) (a, stepsLeft) =
      let sideRoomEntry = Coord 2 (lookupSideRoom a)
          hallway = Coord 1 x
          toHallwayThenSideRoom = manhattan coord hallway + manhattan hallway sideRoomEntry
       in case stepsLeft of
            0 -> 0
            1 -> amphipodCost a * manhattan coord sideRoomEntry
            2 -> amphipodCost a * toHallwayThenSideRoom
            _ -> error "heuristic: unknown stepsleft"

next :: BurrowArr -> SideRooms -> State -> [(Int, Int, State)]
next burrowArr sideRooms (State pods) = map withHeuristics newStates
  where
    withHeuristics (c, s) = (heuristic s, c, s)
    newStates =
      [ (cost, pods')
        | (here, (pod, stepsLeft)) <- Map.toList pods,
          stepsLeft > 0,
          let others = Map.delete here pods,
          let bestSideRoom = sideRoomCoords sideRooms pod pods,
          there <- explore (Map.keysSet pods) burrowArr here,
          isNewCoordOk bestSideRoom stepsLeft there,
          let cost = amphipodCost pod * manhattan here there,
          let pods' = State (Map.insert there (pod, stepsLeft - 1) others)
      ]

explore :: Set Coord -> BurrowArr -> Coord -> [Coord]
explore others burrowArr initC =
  dfs initC $ \c ->
    [ c'
      | c' <- adjacent c,
        Set.notMember c' others,
        burrowArr UArray.! c' /= '#'
    ]

isNewCoordOk :: Maybe Coord -> StepsLeft -> Coord -> Bool
isNewCoordOk bestSideRoom stepsLeft c =
  case stepsLeft of
    1 -> Just c == bestSideRoom
    2 -> c `elem` hallwayCoords
    _ -> error "filterOption: unknown number of stepsLeft"

isValidSolution :: State -> Bool
isValidSolution (State pods) = all isOk (Map.toList pods)
  where
    isOk (Coord _ x, (pod, 0))
      | lookupSideRoom pod == x = True
    isOk _ = False

main :: IO ()
main = do
  txt <- readInputDay 23
  let m = linesWithCoords (lines txt)
      burrowArr = UArray.array (fromJust (boundingBox (map fst m))) m
      amphipods = [(coord, toPod c) | (coord, c) <- m, isUpper c]
      toPod c = fromJust $ lookup c [('A', A), ('B', B), ('C', C), ('D', D)]
      sideRoomSlots = [coord | (coord, c) <- m, isAlpha c]
      row (Coord y _) = y
      sideRooms = (row (head sideRoomSlots), row (last sideRoomSlots))

  let solutions = astarWith (initState amphipods) id (next burrowArr sideRooms)
  print (find (isValidSolution . fst) solutions)