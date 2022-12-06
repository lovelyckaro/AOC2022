module Main where
import SantaLib
import Data.List (nub)

detectStart :: Int -> String -> Int
detectStart wantedLength = go wantedLength "" 0
  where
    go wantedLength window readMessages stream
      | length (nub window) == wantedLength = readMessages
      | length window < wantedLength = go wantedLength (head stream : window) (readMessages+1) (tail stream)
      | otherwise = go wantedLength (head stream : init window) (readMessages+1) (tail stream)


part1 :: String -> Int
part1 = detectStart 4

part2 :: String -> Int
part2 = detectStart 14

main :: IO ()
main = do
  inp <- getInput 6
  putAnswer 6 Part1 (part1 inp)
  putAnswer 6 Part2 (part2 inp)
