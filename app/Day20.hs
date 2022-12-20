{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as V
import SantaLib

-- treat data as tuple of original index and number, in order to handle duplicate
-- numbers correctly
pInp :: String -> Vector (Int, Int)
pInp = V.fromList . zip [0 ..] . map read . lines

insert :: Int -> a -> Vector a -> Vector a
insert i x xs = before <> V.cons x after
  where
    (before, after) = V.splitAt i xs

remove :: Int -> Vector a -> Vector a
remove i xs = before <> after
  where
    before = V.take i xs
    after = V.drop (i + 1) xs

mix :: Vector (Int, Int) -> Vector (Int, Int)
mix v = mixHelper (length v) v v

mixHelper :: Int -> Vector (Int, Int) -> Vector (Int, Int) -> Vector (Int, Int)
mixHelper len original mixed = case V.uncons original of
  Nothing -> mixed
  Just (x, rest) -> case V.elemIndex x mixed of
    Nothing -> error "We've lost an element while mixing..."
    Just i ->
      let i' = 1 + ((i + snd x - 1) `mod` (len - 1))
          mixed' = insert i' x (remove i mixed)
       in mixHelper len rest mixed'

mixTen :: Vector (Int, Int) -> Vector (Int, Int)
mixTen v = iterate (mixHelper (length v) v) v !! 10

part1 :: String -> Int
part1 inp = sum $ map (snd . (mixed V.!)) indices
  where
    v = pInp inp
    len = length v
    mixed = mix v
    start = fromJust $ V.findIndex ((== 0) . snd) mixed
    indices = [(start + offset) `mod` len | offset <- [1000, 2000, 3000]]

part2 :: String -> Int
part2 inp = sum $ map (snd . (mixed V.!)) indices
  where
    key = 811589153
    v = V.map (\(i, x) -> (i, x * key)) $ pInp inp
    len = length v
    mixed = mixTen v
    start = fromJust $ V.findIndex ((== 0) . snd) mixed
    indices = [(start + offset) `mod` len | offset <- [1000, 2000, 3000]]

main :: IO ()
main = do
  inp <- getInput 20
  putAnswer 20 Part1 (part1 inp)
  putAnswer 20 Part2 (part2 inp)
