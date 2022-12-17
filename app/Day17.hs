{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Exts
import SantaLib

data Direction = L | R
  deriving (Show)

pInp :: String -> [Direction]
pInp = cycle . map pChar . filter (/= '\n')
  where
    pChar '>' = R
    pChar '<' = L
    pChar x = error $ "Bad character " <> show x

data Point = Point {x, y :: Int}
  deriving (Show, Eq)

instance Ord Point where
  compare p1 p2 = case comparing y p1 p2 of
    EQ -> comparing x p1 p2
    x -> x

type Grid = Set Point

type Rock = Grid

fromTup :: (Int, Int) -> Point
fromTup (x, y) = Point x y

line, plus, bend, column, square :: Rock
line = S.fromList $ map fromTup [(0, 0), (1, 0), (2, 0), (3, 0)]
plus = S.fromList $ map fromTup [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
bend = S.fromList $ map fromTup [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
column = S.fromList $ map fromTup [(0, 0), (0, 1), (0, 2), (0, 3)]
square = S.fromList $ map fromTup [(0, 0), (0, 1), (1, 0), (1, 1)]

rocks :: [Rock]
rocks = cycle [line, plus, bend, column, square]

down :: Rock -> Rock
down = S.map (\(Point x y) -> Point x (y - 1))

right :: Rock -> Rock
right = S.map (\(Point x y) -> Point (x + 1) y)

left :: Rock -> Rock
left = S.map (\(Point x y) -> Point (x - 1) y)

inBounds :: Rock -> Bool
inBounds = all (\(Point x y) -> x >= 0 && y >= 0 && x < 7)

data GridState = GridState
  { grid :: Grid,
    rock :: Rock,
    queue :: [Rock],
    directions :: [Direction],
    rockNum :: Int
  }

pretty :: Grid -> Rock -> String
pretty grid rock = unlines $ reverse $ chunksOf 7 $ map toChar possibles
  where
    toChar p
      | p `S.member` rock = '@'
      | p `S.member` grid = '#'
      | otherwise = '.'
    (Point _ rows) = S.findMax rock

    possibles = [Point col row | row <- [0 .. rows], col <- [0 .. 6]]

instance Show GridState where
  show gs = pretty gs.grid gs.rock

mkGridState :: [Direction] -> GridState
mkGridState dirs =
  GridState
    { grid = S.empty,
      rock = S.map (\(Point x y) -> Point (x + 2) (y + 3)) (head rocks),
      queue = tail rocks,
      directions = dirs,
      rockNum = 0
    }

overlaps :: Rock -> Grid -> Bool
overlaps rock grid = not $ S.null $ S.intersection rock grid

combine :: Rock -> Grid -> Grid
combine = S.union

highestPoint :: Grid -> Point
highestPoint = S.findMax

move :: Rock -> Grid -> Direction -> Rock
move rock grid dir =
  if inBounds rock' && not (overlaps grid rock')
    then rock'
    else rock
  where
    rock' = case dir of
      R -> right rock
      L -> left rock

moveDown :: Rock -> Grid -> Maybe Rock
moveDown rock grid = do
  let rock' = down rock
  guard (inBounds rock')
  guard (not (overlaps rock' grid))
  return rock'

toStartPos :: Rock -> Grid -> Rock
toStartPos rock grid = S.map (\(Point x y) -> Point (x + 2) (y + p.y + 4)) rock
  where
    p = highestPoint grid

tick :: GridState -> GridState
tick gs =
  let rock' = move gs.rock gs.grid (head gs.directions)
   in case moveDown rock' gs.grid of
        Just rock'' -> gs {rock = rock'', directions = tail gs.directions}
        Nothing ->
          let grid' = combine rock' gs.grid
           in gs
                { rock = toStartPos (head gs.queue) grid',
                  grid = grid',
                  queue = tail gs.queue,
                  directions = tail gs.directions,
                  rockNum = gs.rockNum + 1
                }

iterateUntil :: (t -> Bool) -> (t -> t) -> t -> t
iterateUntil pred f init
  | pred init = init
  | otherwise = iterateUntil pred f (f init)

part1 :: [Direction] -> Int
part1 dirs = succ $ y $ highestPoint $ grid $ iterateUntil ((== 2022) . rockNum) tick (mkGridState dirs)

main :: IO ()
main = do
  inp <- getInput 17
  let moves = pInp inp
  putAnswer 17 Part1 (part1 moves)

-- putAnswer 17 Part2 (part2 moves)
