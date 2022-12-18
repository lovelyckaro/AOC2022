module Main where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing hiding (empty)

type Name = String

type FlowMap = Map Name Integer

type DestMap = Map Name [Name]

pValve :: Parser (FlowMap, DestMap)
pValve = do
  symbol "Valve"
  name <- lexeme $ some upperChar
  mapM_ symbol ["has", "flow", "rate", "="]
  flowrate <- lexeme decimal
  symbol ";"
  try (symbol "tunnels" >> symbol "lead") <|> (symbol "tunnel" >> symbol "leads")
  symbol "to"
  try (symbol "valves") <|> symbol "valve"
  tunnels <- some upperChar `sepBy` symbol ","
  return (M.singleton name flowrate, M.singleton name tunnels)

pInp :: Parser (FlowMap, DestMap)
pInp = do
  ms <- some $ lexemeLn pValve
  let (flows, dests) = unzip ms
  return (foldr M.union M.empty flows, foldr M.union M.empty dests)

-- Just the wikipedia def. of the Floyd-Warshall algorithm
-- https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
pairwiseDistances :: DestMap -> Map (Name, Name) Integer
pairwiseDistances dests = execState go M.empty
  where
    go = do
      let edges = do
            (from, tos) <- M.toList dests
            zip (repeat from) tos
          vertices = M.keys dests
      forM_ vertices $ \vertix ->
        modify $ M.insert (vertix, vertix) 0
      forM_ edges $ \edge ->
        modify $ M.insert edge 1
      forM_ vertices $ \k ->
        forM_ vertices $ \i ->
          forM_ vertices $ \j -> do
            distik <- gets (M.!? (i, k))
            distkj <- gets (M.!? (k, j))
            case (distik, distkj) of
              (Just dist1, Just dist2) -> modify $ M.insertWith min (i, j) (dist1 + dist2)
              _ -> return ()

-- | Approach shamelessly stolen from this guy:
-- https://topaz.github.io/paste/#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
search :: Map (Name, Name) Integer -> FlowMap -> Integer -> Map (Set Name) Integer
search distances flowMap time = execState (go time S.empty 0 "AA") M.empty
  where
    go timeLeft openValves currentFlow position
      | timeLeft <= 0 = return ()
      | otherwise = do
          let cache Nothing = Just currentFlow
              cache (Just cachedFlow) = Just (max cachedFlow currentFlow)
          modify $ M.alter cache openValves
          let nonzero = M.filter (> 0) flowMap
              unvisited = filter (`S.notMember` openValves) (M.keys nonzero)
          forM_ unvisited $ \valve -> do
            let timeLeft' = timeLeft - fromJust (distances M.!? (position, valve)) - 1
                currentFlow' = currentFlow + timeLeft' * fromJust (flowMap M.!? valve)
            go timeLeft' (S.insert valve openValves) currentFlow' valve

part1 :: (FlowMap, DestMap) -> Integer
part1 (flow, dest) = maximum $ search dist flow 30
  where
    dist = pairwiseDistances dest

part2 :: (FlowMap, DestMap) -> Integer
part2 (flow, dest) = maximum $ do
  let dist = pairwiseDistances dest
      cache = search dist flow 26
  (mySet, myFlow) <- M.toList cache
  (elephantSet, elephantFlow) <- M.toList cache
  guard (S.disjoint mySet elephantSet)
  return (myFlow + elephantFlow)

main :: IO ()
main = do
  inp <- getInput 16
  valves <- parseIO pInp "day16.input" inp
  putAnswer 16 Part1 (part1 valves)
  putAnswer 16 Part2 (part2 valves)
