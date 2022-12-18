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
