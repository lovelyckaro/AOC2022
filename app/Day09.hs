module Main where

import Data.Complex (Complex (..), realPart)
import Data.List (nub)
import SantaLib
import SantaLib.Parsing hiding (State)
import System.Exit (exitFailure)
import Prelude hiding (Left, Right)

data Instruction = Right | Left | Up | Down
  deriving (Show)

type Point = Complex Double

pInstruction :: Parser [Instruction]
pInstruction = dir Right "R" <|> dir Left "L" <|> dir Up "U" <|> dir Down "D"
  where
    dir con sym = do
      symbol sym
      amount <- lexemeLn decimal
      return (replicate amount con)

pInp :: Parser [Instruction]
pInp = concat <$> many pInstruction

newtype State = State {knots :: [Point]}
  deriving (Show)

initState :: State
initState = State [0, 0]

initState2 :: State
initState2 = State (replicate 10 0)

neighbor :: Point -> Point -> Bool
neighbor p1 p2 = realPart (abs (p1 - p2)) <= sqrt 2

moveTail :: Point -> Point -> Point
moveTail head tail
  | neighbor head tail = tail
  | otherwise = tail + fmap signum (head - tail)

dirTup :: Instruction -> Point
dirTup Right = 1 :+ 0
dirTup Left = (-1) :+ 0
dirTup Down = 0 :+ (-1)
dirTup Up = 0 :+ 1

move :: State -> Instruction -> State
move (State knots) dir = State knots'
  where
    knots' = head' : zipWith moveTail knots' (tail knots)
    head' = head knots + dirTup dir

part :: State -> [Instruction] -> Int
part init = length . nub . map (last . knots) . scanl move init

main :: IO ()
main = do
  inp <- getInput 9
  ins <- parseIO pInp "day9.input" inp
  putAnswer 9 Part1 (part initState ins)
  putAnswer 9 Part2 (part initState2 ins)
