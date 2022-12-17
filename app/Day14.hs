module Main where

import Control.Monad
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import SantaLib
import SantaLib.Parsing

type World = IntMap IntSet

union :: World -> World -> World
union = IM.unionWith IS.union

difference :: World -> World -> World
difference w = IM.filter (not . IS.null) . IM.unionWith IS.difference w

pPair :: Parser (Int, Int)
pPair = do
  x <- lexeme decimal
  symbol ","
  y <- lexeme decimal
  return (x, y)

mkWorld :: [(Int, Int)] -> World
mkWorld = foldr insertPoint IM.empty
  where
    insertPoint (x, y) = IM.insertWith IS.union x (IS.singleton y)

generatePoints :: [(Int, Int)] -> [(Int, Int)]
generatePoints [] = []
generatePoints [x] = [x]
generatePoints ((xStart, yStart) : (xEnd, yEnd) : rest) =
  [(x, y) | x <- xs, y <- ys] <> generatePoints ((xEnd, yEnd) : rest)
  where
    step n = if n < 0 then -1 else 1
    xStep = step (xEnd - xStart)
    yStep = step (yEnd - yStart)
    xs = [xStart, xStart + xStep .. xEnd]
    ys = [yStart, yStart + yStep .. yEnd]

pLine :: Parser World
pLine = mkWorld . generatePoints <$> (pPair `sepBy` symbol "->")

pInp :: Parser World
pInp = do
  ls <- some $ lexemeLn pLine
  return $ foldr union IM.empty ls

member :: (Int, Int) -> World -> Bool
member (x, y) world = case IM.lookup x world of
  Nothing -> False
  Just column -> IS.member y column

data BadFall = FellOff | Occupied
  deriving (Show)

landingSite :: (Int, Int) -> World -> Either BadFall (Int, Int)
landingSite p world | p `member` world = Left Occupied
landingSite (x, y) world = do
  column <- case IM.lookup x world of
    Nothing -> Left FellOff
    Just c -> Right c
  firstLanding <- case IS.lookupGT y column of
    Nothing -> Left FellOff
    Just y -> Right $ pred y
  let leftDiag = (x - 1, firstLanding + 1)
  let rightDiag = (x + 1, firstLanding + 1)
  case landingSite leftDiag world of
    Right p -> return p
    Left FellOff -> Left FellOff
    Left Occupied -> case landingSite rightDiag world of
      Right p -> return p
      Left FellOff -> Left FellOff
      Left Occupied -> return (x, firstLanding)

tickWorld :: World -> Maybe World
tickWorld w = do
  (x, y) <- case landingSite (500, 0) w of
    Left _ -> Nothing
    Right p -> Just p
  return $ IM.insertWith IS.union x (IS.singleton y) w

run :: World -> World
run w = maybe w run (tickWorld w)

prettyWorld :: World -> String
prettyWorld w = unlines $ chunksOf (xMax - xMin + 1) (map getChar points)
  where
    getChar (x, y) = case IM.lookup x w of
      Nothing -> '.'
      Just is -> if IS.member y is then '#' else '.'
    (xMin, _) = IM.findMin w
    (xMax, _) = IM.findMax w
    yMin = minimum $ IM.map IS.findMin w
    yMax = maximum $ IM.map IS.findMax w
    points = [(x, y) | y <- [yMin .. yMax], x <- [xMin .. xMax]]

part1 :: World -> Int
part1 startingWorld = sum $ IM.map IS.size sand
  where
    finishedWorld = run startingWorld
    sand = difference finishedWorld startingWorld

part2 :: World -> Int
part2 startingWorld = sum $ IM.map IS.size sand
  where
    floorLevel = 2 + maximum (IM.map IS.findMax startingWorld)
    floorNode = IS.singleton floorLevel
    floor = IM.fromList $ zip [-1000 .. 1000] (repeat floorNode)
    startingWorld' = startingWorld `union` floor
    finishedWorld = run startingWorld'
    sand = difference finishedWorld startingWorld'

main :: IO ()
main = do
  inp <- getInput 14
  world <- parseIO pInp "day14.input" inp
  putAnswer 14 Part1 (part1 world)
  putAnswer 14 Part2 (part2 world)
