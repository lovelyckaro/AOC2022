module Main where

import Data.List (nub)
import SantaLib

detectStart :: Int -> String -> Int
detectStart len str
  | take len str == nub (take len str) = len
  | otherwise = 1 + detectStart len (tail str)

part1 :: String -> Int
part1 = detectStart 4

part2 :: String -> Int
part2 = detectStart 14

main :: IO ()
main = do
  inp <- getInput 6
  putAnswer 6 Part1 (part1 inp)
  putAnswer 6 Part2 (part2 inp)
