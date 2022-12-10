module Main where

import Data.List.Split
import SantaLib
import SantaLib.Parsing hiding (State)

data State = State {x :: Int, clk :: Int}
  deriving (Show)

initState = State 1 1

data Instr = Add Int | Nop
  deriving (Show)

pInp :: Parser [Instr]
pInp = some (pAdd <|> pNop)
  where
    pAdd = Add <$> (symbol "addx" >> lexemeLn (signed decimal))
    pNop = Nop <$ symbolLn "noop"

tick :: [State] -> Instr -> [State]
tick prevs Nop = [(last prevs) {clk = clk (last prevs) + 1}]
tick prevs (Add n) = [State (x s) (clk s + 1), State (x s + n) (clk s + 2)]
  where
    s = last prevs

part1pred :: State -> Bool
part1pred s = (clk s - 20) `mod` 40 == 0

part1 :: [Instr] -> Int
part1 = sum . map (\(State x clk) -> x * clk) . filter part1pred . concat . scanl tick [initState]

sprite :: State -> [Int]
sprite s = [x s - 1, x s, x s + 1]

part2 :: [Instr] -> String
part2 instrs = unlines $ chunksOf 40 $ zipWith draw states pixels
  where
    draw s i
      | i `elem` sprite s = '■'
      | otherwise = ' '
    pixels = cycle [0 .. 39]
    states = concat . scanl tick [initState] $ instrs

main :: IO ()
main = do
  inp <- getInput 10
  ins <- parseIO pInp "day10.input" inp
  putAnswer 10 Part1 (part1 ins)
  writeFile "answer/day10-part2" (part2 ins)
