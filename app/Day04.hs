{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (mapMaybe)
import SantaLib

data Range = Range {start, end :: Int}
  deriving (Eq)

instance Show Range where
  show :: Range -> String
  show r = show r.start <> "-" <> show r.end

pRange :: String -> Range
pRange str = Range (read start) (read end)
  where
    [start, end] = splitOn "-" str

overlap :: Range -> Range -> Maybe Range
overlap r1 r2
  | r2.end < r1.start || r1.end < r2.start = Nothing
  | otherwise = Just $ Range (max r1.start r2.start) (min r1.end r2.end)

fullyContains :: Range -> Range -> Bool
fullyContains r1 r2 = case overlap r1 r2 of
  Just res -> res == r1 || res == r2
  Nothing -> False

pInp :: String -> [(Range, Range)]
pInp = map pLine . lines
  where
    pLine = bimap pRange (pRange . tail) . break (== ',')

part1 :: String -> Int
part1 = length . filter (uncurry fullyContains) . pInp

part2 :: String -> Int
part2 = length . mapMaybe (uncurry overlap) . pInp

main :: IO ()
main = do
  inp <- getInput 4
  putAnswer 4 Part1 (part1 inp)
  putAnswer 4 Part2 (part2 inp)
