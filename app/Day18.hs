module Main where

import Control.Monad
import Data.Graph.Inductive hiding (neighbors)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V
import SantaLib
import SantaLib.Parsing hiding (empty)

type Point = (Int, Int, Int)

pInp :: Parser [Point]
pInp = some $ lexemeLn $ do
  [x, y, z] <- lexeme decimal `sepBy` symbol ","
  pure (x, y, z)

neighbors :: Point -> [Point]
neighbors (x, y, z) =
  [ (x + 1, y, z),
    (x - 1, y, z),
    (x, y + 1, z),
    (x, y - 1, z),
    (x, y, z + 1),
    (x, y, z - 1)
  ]

part1 :: Set Point -> Int
part1 points = S.foldr (countPoint points) 0 points
  where
    countPoint set p curr = curr + 6 - length [neighb | neighb <- neighbors p, neighb `S.member` set]

-- part2: Idea, find min and max x y z and limit search to this area. Start bfs
-- in the air outside the rock. Use this to build a set of air blocks, use
-- modified version of part1 on these.

type Bounds = (Int, Int, Int, Int, Int, Int)

bounds :: Set Point -> Bounds
bounds s = (xMin - 1, xMax + 1, yMin - 1, yMax + 1, zMin - 1, zMax + 1)
  where
    xMin = S.findMin . S.map (\(x, _, _) -> x) $ s
    xMax = S.findMax . S.map (\(x, _, _) -> x) $ s
    yMin = S.findMin . S.map (\(_, y, _) -> y) $ s
    yMax = S.findMax . S.map (\(_, y, _) -> y) $ s
    zMin = S.findMin . S.map (\(_, _, z) -> z) $ s
    zMax = S.findMax . S.map (\(_, _, z) -> z) $ s

inBounds :: Bounds -> Point -> Bool
inBounds (xMin, xMax, yMin, yMax, zMin, zMax) (x, y, z) =
  x >= xMin && x <= xMax && y >= yMin && y <= yMax && z >= zMin && z <= zMax

-- Each node is a point, two nodes are connected if they are both air, and in bounds
toGraph :: Set Point -> Gr Point ()
toGraph rock = run_ empty $ do
  let b@(xMin, xMax, yMin, yMax, zMin, zMax) = bounds rock
  nodes <- insMapNodesM [(x, y, z) | x <- [xMin .. xMax], y <- [yMin .. yMax], z <- [zMin .. zMax]]
  forM_ nodes $ \(n, p) -> do
    when (S.notMember p rock) $
      forM_ (neighbors p) $ \neighb -> do
        when (inBounds b neighb && S.notMember neighb rock) (insMapEdgeM (p, neighb, ()))

outsideAir :: Set Point -> Set Point
outsideAir = S.fromList . bfsWith (\(_, _, p, _) -> p) 1 . toGraph

part2 :: Set Point -> Int
part2 set = S.foldr countPoint 0 set
  where
    countPoint p curr = curr + length [neighb | neighb <- neighbors p, S.member neighb air]
    air = outsideAir set

main :: IO ()
main = do
  inp <- getInput 18
  points <- S.fromList <$> parseIO pInp "day18.input" inp
  putAnswer 18 Part1 (part1 points)
  putAnswer 18 Part2 (part2 points)
