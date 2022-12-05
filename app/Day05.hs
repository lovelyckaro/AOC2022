module Main where

import Data.Char (isSpace)
import Data.List (foldl', transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import SantaLib

everyNth :: Int -> [a] -> [a]
everyNth n ls = [ls !! i | i <- [0, n .. length ls - 1]]

pStacks :: [String] -> V.Vector String
pStacks = V.fromList . map (filter (not . isSpace)) . transpose . map (everyNth 4 . tail)

data MoveInstr = MoveInstr {amount, from, to :: Int}
  deriving (Show, Eq)

pInstrs :: [String] -> [MoveInstr]
pInstrs ls = do
  line <- ls
  let [amount, to, from] = filterNums line
  pure $ MoveInstr amount (to - 1) (from - 1)

pInp :: String -> (V.Vector String, [MoveInstr])
pInp inp = (stacks, instrs)
  where
    stacks = pStacks . init . lines $ stackPart
    instrs = pInstrs . lines $ instrPart
    [stackPart, instrPart] = splitOn "\n\n" inp

move :: V.Vector [a] -> MoveInstr -> V.Vector [a]
move s (MoveInstr n from to) =
  V.modify
    ( \v -> do
        fromStack <- MV.read v from
        let moved = take n fromStack
        MV.modify v (drop n) from
        MV.modify v (reverse moved <>) to
    )
    s

move2 :: V.Vector [a] -> MoveInstr -> V.Vector [a]
move2 s (MoveInstr n from to) =
  V.modify
    ( \v -> do
        fromStack <- MV.read v from
        let moved = take n fromStack
        MV.modify v (drop n) from
        MV.modify v (moved <>) to
    )
    s

moveAll :: V.Vector [a] -> [MoveInstr] -> V.Vector [a]
moveAll = foldl' move

moveAll2 :: V.Vector [a] -> [MoveInstr] -> V.Vector [a]
moveAll2 = foldl' move2

topOfStacks :: V.Vector [a] -> [a]
topOfStacks = catMaybes . V.toList . V.map listToMaybe

part1 :: String -> String
part1 = topOfStacks . uncurry moveAll . pInp

part2 :: String -> String
part2 = topOfStacks . uncurry moveAll2 . pInp

main :: IO ()
main = do
  inp <- getInput 5
  writeFile "answer/day5-part1" (part1 inp)
  writeFile "answer/day5-part2" (part2 inp)
