{-# LANGUAGE BangPatterns #-}

module Search where

import Data.Foldable (foldl')
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set

bfs :: Ord s => s -> (s -> [s]) -> [s]
bfs initS = bfsWith initS id

bfsWith :: Ord r => s -> (s -> r) -> (s -> [s]) -> [s]
bfsWith initS rep next = go mempty [initS]
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member (rep x) seen = go seen xs
      | otherwise =
        let next' = filter ((`Set.notMember` seen) . rep) (next x)
         in x : go (Set.insert (rep x) seen) (xs ++ next')

{-# INLINE dijkstraWith #-}
dijkstraWith :: Ord r => s -> (s -> r) -> (s -> [(Int, s)]) -> [(s, Int)]
dijkstraWith initS rep next = go Set.empty (PQ.singleton 0 (initS, 0))
  where
    go seen0 queue0 =
      case PQ.minView queue0 of
        Nothing -> []
        Just ((s0, cost0), q')
          | Set.member r seen0 -> go seen0 q'
          | otherwise -> (s0, cost0) : go seen' q''
          where
            r = rep s0
            seen' = Set.insert r seen0
            q'' = foldl' (\q (k, v) -> PQ.insert k v q) q' nextq
            nextq =
              [ (cost', (s', cost'))
                | (cost1, s') <- next s0,
                  let !cost' = cost0 + cost1
              ]

{-# INLINE astarWith #-}
astarWith :: Ord r => s -> (s -> r) -> (s -> [(Int, Int, s)]) -> [(s, Int)]
astarWith initS rep next = go Set.empty (PQ.singleton 0 (initS, 0))
  where
    go seen0 queue0 =
      case PQ.minView queue0 of
        Nothing -> []
        Just ((s0, cost0), q')
          | Set.member r seen0 -> go seen0 q'
          | otherwise -> (s0, cost0) : go seen' q''
          where
            r = rep s0
            seen' = Set.insert r seen0
            q'' = foldl' (\q (k, v) -> PQ.insert k v q) q' nextq
            nextq =
              [ (cost' + heuristics, (s', cost'))
                | (heuristics, cost1, s') <- next s0,
                  let !cost' = cost0 + cost1
              ]
