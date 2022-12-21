{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.SBV
import SantaLib
import SantaLib.Parsing

type Value = SInteger

data BinOp = Add | Sub | Mul | Div
  deriving (Show)

data Assignment = LitAss String Value | BinOpAss String String BinOp String
  deriving (Show)

type Env = Map String Value

pBinOp :: Parser BinOp
pBinOp =
  Add <$ symbol "+"
    <|> Sub <$ symbol "-"
    <|> Mul <$ symbol "*"
    <|> Div <$ symbol "/"

pInp :: Parser [Assignment]
pInp = some $ lexemeLn $ do
  val <- lexeme (some lowerChar)
  symbol ":"
  LitAss val <$> lexeme decimal <|> BinOpAss val <$> lexeme (some lowerChar) <*> lexeme pBinOp <*> lexeme (some lowerChar)

getOrCreate :: String -> StateT Env Symbolic Value
getOrCreate s =
  gets (M.!? s) >>= \case
    Just v -> return v
    Nothing -> do
      v <- lift $ free s
      modify $ M.insert s v
      return v

handleAssignment :: Assignment -> StateT Env Symbolic ()
handleAssignment (LitAss n v) = do
  var <- getOrCreate n
  lift $ constrain $ var .== v
handleAssignment (BinOpAss name arg1 op arg2) = do
  var <- getOrCreate name
  argVar1 <- getOrCreate arg1
  argVar2 <- getOrCreate arg2
  case op of
    Add -> lift $ constrain $ var .== argVar1 + argVar2
    Sub -> lift $ constrain $ var .== argVar1 - argVar2
    Mul -> lift $ constrain $ var .== argVar1 * argVar2
    Div -> lift $ constrain $ var .== argVar1 `sDiv` argVar2

handleAssignment2 :: Assignment -> StateT Env Symbolic ()
handleAssignment2 (LitAss "humn" _) = void $ getOrCreate "humn"
handleAssignment2 (BinOpAss "root" arg1 _ arg2) = do
  argVar1 <- getOrCreate arg1
  argVar2 <- getOrCreate arg2
  lift $ constrain $ argVar1 .== argVar2
handleAssignment2 x = handleAssignment x

part1 :: [Assignment] -> IO Integer
part1 assignments = do
  res <- satWith z3 {isNonModelVar = (/= "root")} $ flip evalStateT M.empty $ mapM_ handleAssignment assignments
  return $ fromJust $ getModelValue "root" res

part2 :: [Assignment] -> IO Integer
part2 assignments = do
  res <- satWith z3 {isNonModelVar = (/= "humn")} $ flip evalStateT M.empty $ mapM_ handleAssignment2 assignments
  return $ fromJust $ getModelValue "humn" res

main :: IO ()
main = do
  inp <- getInput 21
  assignments <- parseIO pInp "day21.input" inp
  part1 assignments >>= putAnswer 21 Part1
  part2 assignments >>= putAnswer 22 Part2
