module Main where

import Data.Char (isSpace)
import Data.List (foldl', transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, listToMaybe)
import SantaLib.Parsing
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import SantaLib
import System.Exit

data MoveInstr = MoveInstr {amount, from, to :: Int}
  deriving (Show, Eq)

pInstr :: Parser MoveInstr
pInstr = do
  symbol "move"
  amount <- lexeme decimal
  symbol "from"
  from <- pred <$> lexeme decimal
  symbol "to"
  to <- pred <$> lexeme decimal
  return $ MoveInstr amount from to

brackets :: Parser a -> Parser a
brackets = between (string "[") (string "]")

pCell :: Parser (Maybe Char)
pCell = filledCell <|> emptyCell
  where
    filledCell = Just <$> brackets anySingle
    emptyCell = Nothing <$ string "   "

pLine :: Parser [Maybe Char]
pLine = some . lexemeSp $ pCell

pMatrix :: Parser (V.Vector [Char])
pMatrix = do
  ls <- many $ lexemeLn pLine 
  space
  some $ lexeme digitChar
  let stacks = V.fromList . map catMaybes $ transpose ls
  return stacks

pInp :: Parser (V.Vector [Char], [MoveInstr])
pInp = do
  m <- pMatrix
  eol >> eol
  instrs <- some $ lexemeLn pInstr
  eof
  return (m, instrs)  

move :: V.Vector [a] -> MoveInstr -> V.Vector [a]
move s (MoveInstr n from to) =
  V.modify
    ( \v -> do
        fromStack <- MV.read v from
        let moved = take n fromStack
        MV.modify v (drop n) from
        MV.modify v (reverse moved <>) to
    )
    s

move2 :: V.Vector [a] -> MoveInstr -> V.Vector [a]
move2 s (MoveInstr n from to) =
  V.modify
    ( \v -> do
        fromStack <- MV.read v from
        let moved = take n fromStack
        MV.modify v (drop n) from
        MV.modify v (moved <>) to
    )
    s

topOfStacks :: V.Vector [a] -> [a]
topOfStacks = catMaybes . V.toList . V.map listToMaybe

part1 :: V.Vector [Char] -> [MoveInstr] -> String
part1 stacks = topOfStacks . foldl' move stacks

part2 :: V.Vector [Char] -> [MoveInstr] -> String
part2 stacks = topOfStacks . foldl' move2 stacks

main :: IO ()
main = do
  inp <- getInput 5
  (stacks, moves) <- case runParser pInp "input/day5.input" inp of
    Left error -> print (errorBundlePretty error) >> exitFailure
    Right a -> return a
  writeFile "answer/day5-part1" (part1 stacks moves)
  writeFile "answer/day5-part2" (part2 stacks moves)