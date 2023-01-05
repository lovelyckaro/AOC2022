{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import Prelude hiding (Either (..))

data Direction = Up | Down | Left | Right
  deriving (Show, Eq, Ord, Enum)

data Blizzard = Blizzard {x, y :: Int, direction :: Direction}
  deriving (Show, Eq, Ord)

data Valley = Valley
  { blizzards :: Set Blizzard,
    width, height :: Int,
    walls :: Set (Int, Int)
  }
  deriving (Show)

occupied :: Valley -> Int -> Set (Int, Int)
occupied v ticks = S.map toPos v.blizzards
  where
    toPos b = case b.direction of
      Up -> (b.x, (b.y - ticks) `mod` v.height)
      Down -> (b.x, (b.y + ticks) `mod` v.height)
      Left -> ((b.x - ticks) `mod` v.width, b.y)
      Right -> ((b.x + ticks) `mod` v.width, b.y)

pInp :: String -> Valley
pInp inp = Valley blizzards (width - 2) (height - 2) walls
  where
    ls = lines inp
    width = length (head ls)
    height = length ls
    points = [(x, y) | y <- [(-1) .. height - 2], x <- [(-1) .. width - 2]]
    pointChars = zip points (concat ls)
    insBlizzard ((x, y), c) = case c of
      '>' -> S.insert (Blizzard x y Right)
      '<' -> S.insert (Blizzard x y Left)
      '^' -> S.insert (Blizzard x y Up)
      'v' -> S.insert (Blizzard x y Down)
      _ -> id
    blizzards = foldr insBlizzard S.empty pointChars
    insWall (p, '#') = S.insert p
    insWall (_, _) = id
    walls = foldr insWall (S.fromList [(0, -2), (width - 3, height - 1)]) pointChars

start :: Valley -> (Int, Int)
start v = (0, -1)

end :: Valley -> (Int, Int)
end v = (v.width - 1, v.height)

neighbors :: (Int, Int) -> Set (Int, Int)
neighbors (x, y) = S.fromList [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y), (x, y)]

bfs :: Valley -> (Int, Int) -> Int -> Set (Int, Int) -> Int
bfs v goal minute queue
  | minute > 10000 = error "time out"
  | S.null queue = error $ "Couldn't find path (minute = " <> show minute <> ")"
  | goal `S.member` queue = minute
  | otherwise =
      let minute' = minute + 1
          blocked = occupied v minute'
          queue' = (S.unions (S.map neighbors queue) S.\\ blocked) S.\\ v.walls
       in bfs v goal minute' queue'

part1 :: String -> Int
part1 inp = bfs v (end v) 0 (S.singleton (start v))
  where
    v = pInp inp

part2 :: String -> Int
part2 inp = thereAgain
  where
    v = pInp inp
    there = bfs v (end v) 0 (S.singleton (start v))
    backAgain = bfs v (start v) there (S.singleton (end v))
    thereAgain = bfs v (end v) backAgain (S.singleton (start v))

main :: IO ()
main = do
  inp <- getInput 24
  putAnswer 24 Part1 (part1 inp)
  putAnswer 24 Part2 (part2 inp)
