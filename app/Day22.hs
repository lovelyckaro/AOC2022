{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Char (isNumber)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Foldable (Foldable (foldl'))
import Data.List.Split (chunksOf, splitOn)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib

type Point = Complex Double

instance Ord a => Ord (Complex a) where
  compare p1 p2 = case comparing realPart p1 p2 of
    EQ -> comparing imagPart p1 p2
    x -> x

data Board = Board {opens :: Set Point, walls :: Set Point}

data State = State {position :: Point, facing :: Point}
  deriving (Show)

instance Show Board where
  show (Board {opens, walls}) = unlines $ chunksOf (round width + 1) $ map toChar points
    where
      opensMaxWidth = S.findMax (S.map realPart opens)
      wallMaxWidth = S.findMax (S.map realPart opens)
      width = max opensMaxWidth wallMaxWidth
      opensMaxHeight = S.findMax (S.map imagPart opens)
      wallMaxHeight = S.findMax (S.map imagPart opens)
      height = max opensMaxHeight wallMaxHeight
      points = [x :+ y | y <- [0 .. height], x <- [0 .. width]]
      toChar p
        | S.member p opens = '.'
        | S.member p walls = '#'
        | otherwise = ' '

data Instruction = Forward Double | TurnRight | TurnLeft
  deriving (Show)

pBoard :: String -> Board
pBoard str = foldr insPoint (Board S.empty S.empty) pointChars
  where
    ls = lines str
    height = length ls
    width = maximum (map length ls)
    points = [x :+ y | y <- [0 .. fromIntegral height - 1], x <- [0 .. fromIntegral width - 1]]
    pointChars = zip points (concatMap (padLine width) ls)
    padLine wantedLength l = l <> replicate (wantedLength - length l) ' '
    insPoint (p, char) board = case char of
      ' ' -> board
      '.' -> board {opens = S.insert p board.opens}
      '#' -> board {walls = S.insert p board.walls}

pInstructions :: String -> [Instruction]
pInstructions ins = case pInstruction (filter (/= '\n') ins) of
  Nothing -> []
  Just (instr, rest) -> instr : pInstructions rest
  where
    pInstruction [] = Nothing
    pInstruction ('R' : rest) = Just (TurnRight, rest)
    pInstruction ('L' : rest) = Just (TurnLeft, rest)
    pInstruction str = case span isNumber str of
      (num, rest) -> Just (Forward $ read num, rest)

pInp :: String -> (Board, [Instruction])
pInp str = (pBoard boardPart, pInstructions insPart)
  where
    [boardPart, insPart] = splitOn "\n\n" str

pointIsFree :: Board -> Point -> Bool
pointIsFree b p = S.member p b.opens

pointIsWall :: Board -> Point -> Bool
pointIsWall b p = S.member p b.walls

loopAround1 :: Board -> State -> State
loopAround1 b s = s {position = helper b s s.position}
  where
    helper b s candidate
      | pointIsFree b (candidate - s.facing) || pointIsWall b (candidate - s.facing) = helper b s (candidate - s.facing)
      | otherwise = candidate

up, down, left, right :: Point
up = 0 :+ (-1)
down = 0 :+ 1
left = -1
right = 1

loopAround2XWise :: State -> State
loopAround2XWise (State (x :+ y) _)
  | x == 0 && y >= 150 && y < 200 = State ((y - 100) :+ 0) down
  | x == 0 && y >= 100 && y < 150 = State (50 :+ ((-y) + 149)) right
  | x == 49 && y >= 150 && y < 200 = State ((y - 100) :+ 149) up
  | x == 50 && y >= 0 && y < 50 = State (0 :+ ((-y) + 149)) right
  | x == 50 && y >= 50 && y < 100 = State ((y - 50) :+ 100) down
  | x == 99 && y >= 50 && y < 100 = State ((y + 50) :+ 49) up
  | x == 99 && y >= 100 && y < 150 = State (149 :+ ((-y) + 149)) left
  | x == 149 && y >= 0 && y < 50 = State (99 :+ ((-y) + 149)) left

loopAround2YWise :: State -> State
loopAround2YWise (State (x :+ y) _)
  | y == 0 && x >= 50 && x < 100 = State (0 :+ (x + 100)) right
  | y == 0 && x >= 100 && x < 150 = State ((x - 100) :+ 199) up
  | y == 49 && x >= 100 && x < 150 = State (99 :+ (x - 50)) left
  | y == 100 && x >= 0 && x < 50 = State (50 :+ (x + 50)) right
  | y == 149 && x >= 50 && x < 100 = State (49 :+ (x + 100)) left
  | y == 199 && x >= 0 && x < 50 = State ((x + 100) :+ 0) down

loopAround2 :: Board -> State -> State
loopAround2 _ s@(State pos (0 :+ dy)) = loopAround2YWise s
loopAround2 _ s@(State pos (dx :+ 0)) = loopAround2XWise s

nextPoint :: (Board -> State -> State) -> Board -> State -> Maybe State
nextPoint loopAround b s
  | pointIsFree b (s.position + s.facing) = Just $ s {position = s.position + s.facing}
  | pointIsWall b (s.position + s.facing) = Nothing
  | pointIsFree b (loopAround b s).position = Just $ loopAround b s
  | pointIsWall b (loopAround b s).position = Nothing
  | otherwise = error $ "Looped around to bad point: " <> show (loopAround b s) <> ", from state " <> show s

forward :: (Board -> State -> State) -> Board -> Double -> State -> State
forward loopAround b n s
  | abs n < 0.1 = s
  | otherwise = case nextPoint loopAround b s of
      Nothing -> s
      Just s' -> forward loopAround b (n - 1) s'

step :: (Board -> State -> State) -> Board -> Instruction -> State -> State
step loopAround b instruction state = case instruction of
  TurnLeft -> state {facing = state.facing * (0 :+ (-1))}
  TurnRight -> state {facing = state.facing * (0 :+ 1)}
  Forward n -> forward loopAround b n state

-- | Begin in leftmost open tile in top row
initState :: Board -> State
initState b = State {position = initPos, facing = right}
  where
    initPos = S.findMin $ S.filter ((== 0) . imagPart) $ b.opens

run :: (Board -> State -> State) -> Board -> [Instruction] -> State
run loopAround board = foldl' (flip (step loopAround board)) (initState board)

score :: State -> Int
score (State pos facing) =
  1000 * round (imagPart pos + 1) + 4 * round (realPart pos + 1) + case facing of
    (1 :+ 0) -> 0
    (0 :+ 1) -> 1
    (-1 :+ 0) -> 2
    (0 :+ -1) -> 3
    _ -> error "Bad final facing"

part1 :: String -> Int
part1 = score . uncurry (run loopAround1) . pInp

part2 :: String -> Int
part2 = score . uncurry (run loopAround2) . pInp

main :: IO ()
main = do
  inp <- getInput 22
  putAnswer 22 Part1 (part1 inp)
  putAnswer 22 Part2 (part2 inp)
