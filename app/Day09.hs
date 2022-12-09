{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing hiding (State)
import Prelude hiding (Left, Right)
import Prelude qualified as P
import System.Exit (exitFailure)

data Instruction = Right | Left | Up | Down
  deriving (Show)

pInstruction :: Parser [Instruction]
pInstruction = dir Right "R" <|> dir Left "L" <|> dir Up "U" <|> dir Down "D"
  where
    dir con sym = do
      symbol sym
      amount <- lexemeLn decimal
      return (replicate amount con)

pInp :: Parser [Instruction]
pInp = concat <$> many pInstruction

data State = State {headPos :: (Int, Int), tailPos :: (Int, Int)}
  deriving (Show)

initState :: State
initState = State (0, 0) (0, 0)

addTup :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTup (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subTup :: (Int, Int) -> (Int, Int) -> (Int, Int)
subTup (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

capMove :: (Int, Int) -> (Int, Int)
capMove (x,y) = (x', y')
  where
    x' = if x < 0 then max (-1) x else min 1 x
    y' = if y < 0 then max (-1) y else min 1 y

neighbor :: (Int, Int) -> (Int, Int) -> Bool
neighbor (x,y) (x', y') = abs (x - x') <= 1 && abs (y - y') <= 1

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail head tail 
  | neighbor head tail = tail
  | otherwise = tail `addTup` capMove (head `subTup` tail)

move :: State -> Instruction -> State
move (State {..}) dir = State head' (moveTail head' tailPos)
  where
    head' = headPos `addTup` dirTup dir
    dirTup :: Instruction -> (Int, Int)
    dirTup Right = (1,0)
    dirTup Left = (-1,0)
    dirTup Down = (0,-1)
    dirTup Up = (0,1)


part1 :: [Instruction] -> Int
part1 = S.size . S.fromList . map tailPos . scanl move initState

main :: IO ()
main = do
  inp <- getInput 9
  ins <- case parse pInp "" inp of
    P.Right ok -> return ok
    P.Left err -> putStrLn (errorBundlePretty err) >> exitFailure
  putAnswer 9 Part1 (part1 ins)
