module Main where

import Control.Monad (void)
import Data.List (sort)
import SantaLib
import SantaLib.Parsing

data Packet = IntPacket Int | ListPacket [Packet]
  deriving (Show)

instance Eq Packet where
  (==) :: Packet -> Packet -> Bool
  a == b = case compare a b of
    EQ -> True
    _ -> False

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (IntPacket n1) (IntPacket n2) = compare n1 n2
  compare (ListPacket l1) (ListPacket l2) = case foldl firstNonEq EQ (zipWith compare l1 l2) of
    EQ -> compare (length l1) (length l2)
    x -> x
    where
      firstNonEq :: Ordering -> Ordering -> Ordering
      firstNonEq EQ LT = LT
      firstNonEq EQ GT = GT
      firstNonEq EQ EQ = EQ
      firstNonEq x _ = x
  compare n1@(IntPacket _) l2@(ListPacket _) = compare (ListPacket [n1]) l2
  compare l1@(ListPacket _) n2@(IntPacket _) = compare l1 (ListPacket [n2])

pPacket :: Parser Packet
pPacket = pIntPacket <|> pListPacket
  where
    pIntPacket = IntPacket <$> lexeme decimal
    pListPacket = do
      symbol "["
      packets <- sepBy (lexeme pPacket) (symbol ",")
      symbol "]"
      return $ ListPacket packets

pInp :: Parser [(Packet, Packet)]
pInp = some $ do
  p1 <- lexemeLn pPacket
  p2 <- lexemeLn pPacket
  void eol <|> eof
  pure (p1, p2)

part1 :: [(Packet, Packet)] -> Int
part1 = sum . map fst . filter ((/= GT) . snd) . zip [1 ..] . map (uncurry compare)

part2 :: [(Packet, Packet)] -> Int
part2 = product . map fst . filter ((`elem` dividers) . snd) . zip [1 ..] . sort . addDividers . concatMap (\(p1, p2) -> [p1, p2])
  where
    dividers = [ListPacket [ListPacket [IntPacket 2]], ListPacket [ListPacket [IntPacket 6]]]
    addDividers = (dividers <>)

main :: IO ()
main = do
  inp <- getInput 13
  packets <- parseIO pInp "day13.input" inp
  putAnswer 13 Part1 (part1 packets)
  putAnswer 13 Part2 (part2 packets)
