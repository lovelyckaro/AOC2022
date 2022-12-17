{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (guard)
import Data.Bits
  ( Bits (popCount, shiftL, shiftR, testBit, (.&.), (.|.)),
  )
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Int (Int8)
import GHC.Generics (Generic)
import SantaLib
import Text.Printf (printf)

data Direction = L | R
  deriving (Show, Eq, Ord, Generic)

instance Hashable Direction

pInp :: String -> [Direction]
pInp = map pChar . filter (/= '\n')
  where
    pChar '>' = R
    pChar '<' = L
    pChar x = error $ "Bad character " <> show x

-- | Row is indexed by the positive part of a Int8
newtype Row = Row Int8
  deriving (Eq, Ord, Num, Bits, Hashable)

instance Show Row where
  show (Row n) = printf "%07b" n

type Grid = [Row]

type Rock = Grid

line, plus, bend, column, square :: Rock
line = [0b0011110]
plus =
  [ 0b0001000,
    0b0011100,
    0b0001000
  ]
bend =
  [ 0b0000100,
    0b0000100,
    0b0011100
  ]
column =
  [ 0b0010000,
    0b0010000,
    0b0010000,
    0b0010000
  ]
square =
  [ 0b0011000,
    0b0011000
  ]

rocks :: [Rock]
rocks = [line, plus, bend, column, square]

down :: Rock -> Rock
down = (0b0 :)

right :: Rock -> Rock
right rock =
  if sum (map popCount rock') == sum (map popCount rock)
    then rock'
    else rock
  where
    rock' = map (`shiftR` 1) rock

left :: Rock -> Rock
left rock =
  if sum (map popCount rock') == sum (map popCount rock)
    && not (any (`testBit` 7) rock')
    then rock'
    else rock
  where
    rock' = map (`shiftL` 1) rock

overlaps :: Rock -> Grid -> Bool
overlaps rock grid = sum (zipWith (.&.) rock grid) /= 0

join :: Rock -> Grid -> Grid
join rock grid = zipWith (.|.) rock' grid'
  where
    (rock', grid') = pad rock grid
    pad xs ys = case compare (length xs) (length ys) of
      EQ -> (xs, ys)
      GT -> (xs, ys <> replicate (length xs - length ys) 0)
      LT -> (xs <> replicate (length ys - length xs) 0, ys)

data GridState = GridState
  { grid :: Grid,
    rock :: Rock,
    queue :: ([Rock], [Rock]),
    directions :: ([Direction], [Direction])
  }
  deriving (Eq, Ord, Generic)

instance Hashable GridState

instance Show GridState where
  show gs = show (join gs.rock gs.grid)

moveSide :: Direction -> Grid -> Rock -> Rock
moveSide direction grid rock = if rock' `overlaps` grid then rock else rock'
  where
    rock' = case direction of
      R -> right rock
      L -> left rock

moveDown :: Grid -> Rock -> Maybe Rock
moveDown grid rock = do
  let rock' = down rock
  guard $ not $ rock' `overlaps` grid
  guard $ length rock < length grid
  return rock'

nextDirection :: GridState -> (Direction, ([Direction], [Direction]))
nextDirection gs = case gs.directions of
  ([], dirs) -> (last dirs, (tail (reverse dirs), [last dirs]))
  (dir : dirsLeft, dirsUsed) -> (dir, (dirsLeft, dir : dirsUsed))

nextRock :: GridState -> (Rock, ([Rock], [Rock]))
nextRock gs = case gs.queue of
  ([], rocks) -> (last rocks, (tail (reverse rocks), [last rocks]))
  (rock : rocksLeft, rocksUsed) -> (rock, (rocksLeft, rock : rocksUsed))

padGrid :: Int -> Grid -> Grid
padGrid n = (replicate n 0 <>) . dropWhile (== 0)

tick :: GridState -> GridState
tick gs =
  let (dir, dirs') = nextDirection gs
      rock' = moveSide dir gs.grid gs.rock
   in case moveDown gs.grid rock' of
        Just rock'' -> gs {rock = rock'', directions = dirs'}
        Nothing ->
          let grid' = join rock' gs.grid
              (newRock, queue') = nextRock gs
           in gs
                { rock = newRock,
                  grid = padGrid (length newRock + 3) grid',
                  queue = queue',
                  directions = dirs'
                }

tickRock :: GridState -> (Int, GridState)
tickRock gs = (height ticked - height gs, ticked)
  where
    ticked = helper gs
    helper gs = if gs.grid == gs'.grid then helper gs' else gs'
      where
        gs' = tick gs

mkGridState :: [Direction] -> GridState
mkGridState dirs =
  GridState
    { grid = padGrid (length (head rocks) + 3) [0],
      rock = head rocks,
      queue = (tail rocks, [head rocks]),
      directions = (dirs, [])
    }

-- | Assume a rock won't ever fall more than 100 rows down.
indexItem :: GridState -> GridState
indexItem gs = gs {grid = take 100 gs.grid}

height :: GridState -> Int
height gs = length $ dropWhile (== 0) gs.grid

heightAfterNRocks :: Int -> GridState -> Int
heightAfterNRocks wantedRocks = fst . go 0 wantedRocks HM.empty
  where
    go :: Int -> Int -> HashMap GridState (Int, Int) -> GridState -> (Int, HashMap GridState (Int, Int))
    go currHeight 0 index gridState = (currHeight, index)
    go currHeight rocksLeft index gridState = case index HM.!? indexItem gridState of
      Nothing ->
        let index' = HM.insert (indexItem gridState) (currHeight, rocksLeft) index
            (heightDelta, gridState') = tickRock gridState
         in go (currHeight + heightDelta) (pred rocksLeft) index' gridState'
      -- Cycle detected, we've been here before. A that point the tower was
      -- oldHeight long, and we had oldRocks many rocks left
      Just (oldHeight, oldRocks) ->
        let heightDelta = currHeight - oldHeight
            rockDelta = oldRocks - rocksLeft
            (multiplier, rocksLeft') = divMod rocksLeft rockDelta
            (heightDiff, nextState) = tickRock (indexItem gridState)
         in go (currHeight + multiplier * heightDelta + heightDiff) (rocksLeft' - 1) index nextState

part1 :: [Direction] -> Int
part1 instrs = heightAfterNRocks 2022 (mkGridState instrs)

part2 :: [Direction] -> Int
part2 instrs = heightAfterNRocks 1000000000000 (mkGridState instrs)

main :: IO ()
main = do
  inp <- getInput 17
  let moves = pInp inp
  putAnswer 17 Part1 (part1 moves)
  putAnswer 17 Part2 (part2 moves)
