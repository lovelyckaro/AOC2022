module Main where

import Control.Applicative (Alternative ((<|>)))
import Data.Foldable (Foldable (foldl'))
import Data.List (transpose)
import Data.Maybe (catMaybes)
import SantaLib

pInp :: String -> [[Int]]
pInp = map (map (read . (: []))) . lines

bigger :: Ord a => (a, [Maybe a]) -> a -> (a, [Maybe a])
bigger (prevMax, acc) tree =
  if tree > prevMax
    then (tree, Just tree : acc)
    else (prevMax, Nothing : acc)

visibleFromLeft :: [[Int]] -> [[Maybe Int]]
visibleFromLeft rows = do
  row <- rows
  let (max, row') = foldl' bigger (-1, []) row
  return (reverse row')

visibles :: [[Int]] -> [[Maybe Int]]
visibles rows =
  foldr
    merge
    (replicate numRows (replicate numCols Nothing))
    [ visibleFromLeft rows,
      map reverse $ visibleFromLeft $ map reverse rows,
      transpose $ visibleFromLeft $ transpose rows,
      transpose $ map reverse $ visibleFromLeft $ map reverse $ transpose rows
    ]
  where
    numRows = length rows
    numCols = length (head rows)
    merge :: [[Maybe Int]] -> [[Maybe Int]] -> [[Maybe Int]]
    merge = zipWith (zipWith (<|>))

part1 :: String -> Int
part1 = length . catMaybes . concat . visibles . pInp

at :: (Int, Int) -> [[Int]] -> Int
at (row, col) rows = rows !! row !! col

rightOf :: (Int, Int) -> [[Int]] -> [Int]
rightOf (rowNum, colNum) rows = drop (colNum + 1) (rows !! rowNum)

leftOf :: (Int, Int) -> [[Int]] -> [Int]
leftOf (rowNum, colNum) rows = reverse $ take colNum (rows !! rowNum)

above :: (Int, Int) -> [[Int]] -> [Int]
above (row, col) = leftOf (col, row) . transpose

below :: (Int, Int) -> [[Int]] -> [Int]
below (row, col) = rightOf (col, row) . transpose

score :: (Int, Int) -> [[Int]] -> Int
score pos forest = product $ map walk [rightOf pos forest, leftOf pos forest, above pos forest, below pos forest]
  where
    walk :: [Int] -> Int
    walk candidates =
      min
        (length candidates)
        (succ . length . takeWhile (< at pos forest) $ candidates)

part2 :: String -> Int
part2 str = maximum $ map (`score` inp) $ [(row, col) | row <- [0 .. rows - 1], col <- [0 .. cols - 1]]
  where
    inp = pInp str
    rows = length inp
    cols = length (head inp)

main :: IO ()
main = do
  inp <- getInput 8
  putAnswer 8 Part1 (part1 inp)
  putAnswer 8 Part2 (part2 inp)
