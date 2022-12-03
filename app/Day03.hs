module Main where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Char ( isAsciiLower, isAsciiUpper )
import Data.List.Split ( chunksOf )
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib

pLine :: String -> (Set Char, Set Char)
pLine line = bimap S.fromList S.fromList $ splitAt (length line `div` 2) line

pInp :: String -> [(Set Char, Set Char)]
pInp = map pLine . lines

inBoth :: Set Char -> Set Char -> Char
inBoth s1 s2 = S.findMin (s1 `S.intersection` s2)

priority :: Char -> Int
priority c
  | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
  | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = error "Bad character"

part1 :: String -> Int
part1 = sum . map (priority . uncurry inBoth) . pInp

pInp2 :: String -> [[Set Char]]
pInp2 = map (map S.fromList) . chunksOf 3 . lines

inCommon :: [Set Char] -> Char
inCommon = S.findMin . foldr1 S.intersection

part2 :: String -> Int
part2 = sum . map (priority . inCommon) . pInp2

main :: IO ()
main = do
  inp <- getInput 3
  putAnswer 3 Part1 (part1 inp)
  putAnswer 3 Part2 (part2 inp)
