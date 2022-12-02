module Main where
import SantaLib
import Data.Maybe (fromJust)

data P2Input = X | Y | Z
  deriving (Show, Eq, Ord, Read)

data P1Input = A | B | C
  deriving (Show, Eq, Ord, Read)

choicePoints :: P2Input -> Int
choicePoints X = 1
choicePoints Y = 2
choicePoints Z = 3

p2wins :: [(P1Input, P2Input)]
p2wins = [(A, Y), (B, Z), (C, X)]

p2draws = [(A, X), (B, Y), (C, Z)]

p2loses = [(A, Z), (B, X), (C, Y)]

score :: P1Input -> P2Input -> Int
score p1 p2
  | (p1, p2) `elem` p2wins = 6 + choicePoints p2
  | (p1, p2) `elem` p2draws = 3 + choicePoints p2
  | (p1, p2) `elem` p2loses = 0 + choicePoints p2

part1 :: String -> Int
part1 = sum . map (uncurry score . pLine) . lines
  where
    pLine l = case words l of
      [p1, p2] -> (read p1, read p2)

part2Score :: P1Input -> P2Input -> Int
part2Score p1 X = 0 + choicePoints (fromJust (lookup p1 p2loses))
part2Score p1 Y = 3 + choicePoints (fromJust (lookup p1 p2draws))
part2Score p1 Z = 6 + choicePoints (fromJust (lookup p1 p2wins))


part2 :: String -> Int
part2 = sum . map (uncurry part2Score . pLine) . lines
  where
    pLine l = case words l of
      [p1, p2] -> (read p1, read p2)


main :: IO ()
main = do
  inp <- getInput 2
  putAnswer 2 Part1 (part1 inp)
  putAnswer 2 Part2 (part2 inp)