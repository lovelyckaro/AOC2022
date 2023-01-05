{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import SantaLib

pSnafu :: String -> Int
pSnafu = foldl (\prev d -> prev * 5 + d) 0 . digits
  where
    digits = map \case
      '2' -> 2
      '1' -> 1
      '0' -> 0
      '-' -> -1
      '=' -> -2
      _ -> error "Not a valid snafu digit"

toSnafu :: Int -> String
toSnafu 0 = ""
toSnafu x = toSnafu ((x + 2) `div` 5) <> digit ((x + 2) `mod` 5)
  where
    digit 0 = "="
    digit 1 = "-"
    digit 2 = "0"
    digit 3 = "1"
    digit 4 = "2"

part1 :: String -> String
part1 = toSnafu . sum . map pSnafu . lines

main :: IO ()
main = do
  inp <- getInput 25
  writeFile "answer/day25-part1" (part1 inp)
