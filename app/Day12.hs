module Main where

import Control.Monad
import Data.Char
import Data.Foldable (maximumBy)
import Data.Graph.Inductive hiding (neighbors)
import Data.Graph.Inductive.Query.SP
import Data.Maybe
import Data.Ord
import SantaLib

neighbors :: ((Int, Int) -> Bool) -> (Int, Int) -> [(Int, Int)]
neighbors inBounds (x, y) = do
  p <- [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]
  guard (inBounds p)
  return p

toHeight :: Char -> Char
toHeight 'E' = 'z'
toHeight 'S' = 'a'
toHeight x = x

label1 :: Char -> Char -> Int
label1 _ _ = 1

label2 :: Char -> Char -> Int
label2 'a' 'a' = 0
label2 _ _ = 1

parseInp :: (Char -> Char -> Int) -> String -> ((), (NodeMap (Int, Int), Gr (Int, Int) Int))
parseInp label inp = run empty $ do
  let matrix = lines . map toHeight $ inp
  let width = length (head matrix)
  let height = length matrix
  let inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height
  nodes <- insMapNodesM ((,) <$> [0 .. width - 1] <*> [0 .. height - 1])
  forM_ nodes $ \(n, p@(x, y)) -> do
    let ourChar = matrix !! y !! x
    forM_ (neighbors inBounds p) $ \neighbor@(x', y') -> do
      let neighbChar = matrix !! y' !! x'
      when (neighbChar <= succ ourChar) $ insMapEdgeM (p, neighbor, label ourChar neighbChar)

findPoint :: String -> Char -> (Int, Int)
findPoint inp c = snd $ head $ filter ((== c) . fst) pointChars
  where
    ls = lines inp
    width = length (head ls)
    height = length ls
    points = [(col, row) | row <- [0 .. height - 1], col <- [0 .. width - 1]]
    pointChars = zip (concat ls) points

part1 :: String -> Int
part1 inp = fromJust $ spLength start goal graph
  where
    ((), (nodemap, graph)) = parseInp label1 inp
    (start, _) = mkNode_ nodemap (findPoint inp 'S')
    (goal, _) = mkNode_ nodemap (findPoint inp 'E')

part2 :: String -> Int
part2 inp = fromJust $ spLength start goal graph
  where
    ((), (nodemap, graph)) = parseInp label2 inp
    (start, _) = mkNode_ nodemap (findPoint inp 'S')
    (goal, _) = mkNode_ nodemap (findPoint inp 'E')

main :: IO ()
main = do
  inp <- getInput 12
  putAnswer 12 Part1 (part1 inp)
  putAnswer 12 Part2 (part2 inp)
