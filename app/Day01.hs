module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down))
import SantaLib

pInp :: String -> [[Int]]
pInp = map (map read . lines) . splitOn "\n\n"

part1 :: String -> Int
part1 = maximum . map sum . pInp

part2 :: String -> Int
part2 = sum . take 3 . sortOn Down . map sum . pInp

main :: IO ()
main = do
  inp <- getInput 1
  putAnswer 1 Part1 (part1 inp)
  putAnswer 1 Part2 (part2 inp)
