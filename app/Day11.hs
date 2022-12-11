{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad.State
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import SantaLib
import SantaLib.Parsing hiding (State)

data Monkey = Monkey
  { items :: Seq Int,
    operation :: Int -> Int,
    divBy :: Int,
    destinationTrue :: Int,
    destinationFalse :: Int,
    inspections :: Int
  }

instance Show Monkey where
  show m =
    "Monkey {items = "
      <> show m.items
      <> ", operation = <operation>, "
      <> "divBy = "
      <> show m.divBy
      <> ", destinationTrue = "
      <> show m.destinationTrue
      <> ", destinationFalse = "
      <> show m.destinationFalse
      <> ", inspections = "
      <> show m.inspections
      <> "}"

data Operand = Old | IntLit Int
  deriving (Show)

pItems :: Parser (Seq Int)
pItems = do
  symbol "Starting" >> symbol "items:"
  items <- sepBy (lexeme decimal) (symbol ",")
  eol
  return $ Seq.fromList items

pOperation :: Parser (Int -> Int)
pOperation = do
  symbol "Operation:" >> symbol "new" >> symbol "="
  let pOperand = Old <$ symbol "old" <|> IntLit <$> lexeme decimal
  let pOperator = ((+) <$ symbol "+") <|> ((*) <$ symbol "*")
  op1 <- pOperand
  operator <- pOperator
  op2 <- pOperand
  eol
  return $ \old -> case (op1, op2) of
    (Old, Old) -> operator old old
    (Old, IntLit n) -> operator old n
    (IntLit n, Old) -> operator n old
    (IntLit a, IntLit b) -> operator a b

pDivBy :: Parser Int
pDivBy = do
  symbol "Test:" >> symbol "divisible" >> symbol "by"
  lexemeLn decimal

pDest :: Parser (Bool, Int)
pDest = do
  symbol "If"
  b <- (True <$ symbol "true:") <|> (False <$ symbol "false:")
  mapM_ symbol (words "throw to monkey")
  n <- lexemeLn decimal
  return (b, n)

pMonkey :: Parser (Int, Monkey)
pMonkey = nonIndented spaceLn $ do
  symbol "Monkey"
  n <- lexeme decimal
  symbolLn ":"
  (col, items) <- indented pos1 pItems
  operation <- aligned col pOperation
  divBy <- aligned col pDivBy
  (col', (True, ifTrue)) <- indented col pDest
  (False, ifFalse) <- aligned col' pDest
  return (n, Monkey items operation divBy ifTrue ifFalse 0)

pInp :: Parser (Map Int Monkey)
pInp = M.fromList <$> some pMonkey

tick :: (Int -> Int) -> State (Map Int Monkey) ()
tick worryFun = do
  state <- get
  forM_ (M.keys state) $ \monkeyNum -> do
    monkey <- gets (M.! monkeyNum)
    monkey' <- tickMonkey worryFun monkey
    modify (M.insert monkeyNum monkey')

throw :: Int -> Int -> State (Map Int Monkey) ()
throw monkeyNum item = do
  monkey <- gets (M.! monkeyNum)
  let monkey' = monkey {items = monkey.items <> Seq.singleton item}
  modify (M.insert monkeyNum monkey')

tickMonkey :: (Int -> Int) -> Monkey -> State (Map Int Monkey) Monkey
tickMonkey worryFun monkey = do
  forM_ monkey.items $ \item -> do
    let worry' = monkey.operation item
    let boredWorry' = worryFun worry'
    if boredWorry' `mod` monkey.divBy == 0
      then throw monkey.destinationTrue boredWorry'
      else throw monkey.destinationFalse boredWorry'
  return
    monkey
      { items = Seq.empty,
        inspections = monkey.inspections + fromIntegral (length monkey.items)
      }

part1 :: Int -> Map Int Monkey -> Int
part1 group =
  product
    . take 2
    . sortOn Down
    . map inspections
    . M.elems
    . execState (replicateM_ 20 (tick (`div` 3)))

part2 :: Int -> Map Int Monkey -> Int
part2 group =
  product
    . take 2
    . sortOn Down
    . map inspections
    . M.elems
    . execState (replicateM_ 10000 (tick (`mod` group)))

main :: IO ()
main = do
  inp <- getInput 11
  initMonkeys <- parseIO pInp "day11.input" inp
  -- we are working in group Zp where p is the product of monkey divisors
  let group = product $ map divBy $ M.elems initMonkeys
  putAnswer 11 Part1 (part1 group initMonkeys)
  putAnswer 11 Part2 (part2 group initMonkeys)
