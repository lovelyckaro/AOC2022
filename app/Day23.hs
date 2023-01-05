{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib

data Direction = North | South | West | East
  deriving (Show, Eq, Ord, Enum)

data Elf = Elf {position :: (Int, Int), queue :: [Direction]}
  deriving (Show, Ord, Eq)

pretty :: Set Elf -> String
pretty elfs = unlines $ chunksOf (xMax - xMin + 1) $ map toChar points
  where
    toChar p
      | p `S.member` positions = '#'
      | otherwise = '.'
    positions = S.map position elfs
    xs = S.map fst positions
    xMin = S.findMin xs
    xMax = S.findMax xs
    ys = S.map snd positions
    yMin = S.findMin ys
    yMax = S.findMax ys
    points = [(x, y) | y <- [yMin .. yMax], x <- [xMin .. xMax]]

mkElf :: (Int, Int) -> Elf
mkElf pos = Elf pos [North ..]

pInp :: String -> Set Elf
pInp inp = foldr insPoint S.empty (zip points (concat ls))
  where
    ls = lines inp
    width = length (head ls)
    height = length ls
    points = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    insPoint (p, '#') = S.insert (mkElf p)
    insPoint (p, _) = id

neighbors :: Maybe Direction -> (Int, Int) -> [(Int, Int)]
neighbors mbdir (x, y) = do
  x' <- case mbdir of
    Nothing -> [x - 1, x, x + 1]
    Just North -> [x - 1, x, x + 1]
    Just South -> [x - 1, x, x + 1]
    Just West -> [x - 1]
    Just East -> [x + 1]
  y' <- case mbdir of
    Nothing -> [y - 1, y, y + 1]
    Just North -> [y - 1]
    Just South -> [y + 1]
    Just West -> [y - 1, y, y + 1]
    Just East -> [y - 1, y, y + 1]
  guard $ not $ x == x' && y == y'
  return (x', y')

nextPos :: Maybe Direction -> (Int, Int) -> (Int, Int)
nextPos Nothing p = p
nextPos (Just North) (x, y) = (x, y - 1)
nextPos (Just South) (x, y) = (x, y + 1)
nextPos (Just West) (x, y) = (x - 1, y)
nextPos (Just East) (x, y) = (x + 1, y)

countNeighbors :: Maybe Direction -> Set (Int, Int) -> (Int, Int) -> Int
countNeighbors mbdir set p = foldr countPoint 0 (neighbors mbdir p)
  where
    countPoint p accum = if p `S.member` set then succ accum else accum

-- Nothing signifies no change needed
tick :: Set Elf -> Maybe (Set Elf)
tick set =
  let candidate = half2 (half1 set)
   in if S.map position candidate == S.map position set
        then Nothing
        else Just candidate

-- Set of elf to a map from positions and elfs wanting to move into that position
half1 :: Set Elf -> Map (Int, Int) [Elf]
half1 set = S.foldr tickElf M.empty set
  where
    positions = S.map position set
    tickElf elf m =
      let dirs = Nothing : map Just elf.queue
          nextPoses = map (`nextPos` elf.position) dirs
          neighbCounts = map (\dir -> countNeighbors dir positions elf.position) dirs
          candidates = zip neighbCounts nextPoses
       in case filter ((== 0) . fst) candidates of
            [] -> M.insertWith (<>) elf.position [elf] m
            ((0, pos) : _) -> M.insertWith (<>) pos [elf] m

cycleDirs :: Elf -> Elf
cycleDirs e = e {queue = tail e.queue <> [head e.queue]}

half2 :: Map (Int, Int) [Elf] -> Set Elf
half2 = M.foldrWithKey insElf S.empty
  where
    insElf newPos elfs accum = case elfs of
      [elf] -> S.insert (cycleDirs elf {position = newPos}) accum
      elfs -> foldr (S.insert . cycleDirs) accum elfs

score :: Set Elf -> Int
score elfs = size - S.size (S.map position elfs)
  where
    xs = S.map (fst . position) elfs
    ys = S.map (snd . position) elfs
    xMin = S.findMin xs
    xMax = S.findMax xs
    yMin = S.findMin ys
    yMax = S.findMax ys
    size = (xMax - xMin + 1) * (yMax - yMin + 1)

run1 :: Set Elf -> Set Elf
run1 e = helper 10 e
  where
    helper 0 e = e
    helper n e = case tick e of
      Nothing -> e
      Just e' -> helper (n - 1) e'

run2 :: Set Elf -> (Int, Set Elf)
run2 e = helper 0 e
  where
    helper n e = case tick e of
      Nothing -> (n, e)
      Just e' -> helper (succ n) e'

part1 :: String -> Int
part1 = score . run1 . pInp

part2 :: String -> Int
part2 = succ . fst . run2 . pInp

main :: IO ()
main = do
  inp <- getInput 23
  putAnswer 23 Part1 (part1 inp)
  putAnswer 23 Part2 (part2 inp)
