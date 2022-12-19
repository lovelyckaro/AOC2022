{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.SBV
import SantaLib
import SantaLib.Parsing hiding (State)

type Amount = SWord8

data Material = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord, Enum)

data Blueprint = Blueprint {number :: Int, robots :: Map Material (Map Material Amount)}
  deriving (Show)

pMaterial :: Parser Material
pMaterial =
  Ore <$ symbol "ore"
    <|> Clay <$ symbol "clay"
    <|> Obsidian <$ symbol "obsidian"
    <|> Geode <$ symbol "geode"

pRobot :: Parser (Material, Map Material Amount)
pRobot = do
  symbol "Each"
  material <- pMaterial
  symbol "robot" >> symbol "costs"
  cost <- sepBy ((,) <$> lexeme decimal <*> pMaterial) (symbol "and")
  let costMap = M.fromList $ map (\(x, y) -> (y, x)) cost
  symbol "."
  optional (lexeme eol)
  return (material, costMap)

pBlueprint :: Parser Blueprint
pBlueprint = do
  symbol "Blueprint"
  number <- lexeme decimal
  symbol ":" >> optional (lexeme eol)
  robots <- some pRobot
  optional eol
  return $ Blueprint number (M.fromList robots)

pInp :: Parser [Blueprint]
pInp = some pBlueprint

data RobotState = RobotState
  { materials :: Map Material Amount,
    income :: Map Material Amount
  }
  deriving (Show)

type RobotM = StateT RobotState Symbolic

canAfford :: Blueprint -> Material -> RobotM SBool
canAfford bp robotMaterial = do
  let cost = bp.robots M.! robotMaterial
  available <- gets materials
  let afford = sAnd $ M.elems $ M.intersectionWith (.>=) available cost
  return afford

{-
The problem is modeled by introducing four boolean choices each minute (One for each buying each type of robot).
As well as keeping track of (symbolic) 8 bit values representing the current
amount of resources of each type, as well as the amount of robots of each type.

Each minute is divided into three phases:

First the buying phase:
We are only allowed to buy a robot if we can afford it. We are only allowed to
buy one robot per minute. If we buy a robot the cost of that robot is subtracted
from our available resources.

Then the mining phase. This is just adding materials to our available resources.

Finally in the last phase any robot we bought is added to the available robots.
-}
evalMinute :: Blueprint -> RobotM ()
evalMinute blueprint = do
  -- Buy phase
  buyChoices <- lift $ sBools ["buyOreRobot", "buyClayRobot", "buyObsidianRobot", "buyGeodeRobot"]
  -- Can buy at most one robot
  lift $ constrain $ pbMutexed buyChoices
  forM_ (zip [Ore ..] buyChoices) $ \(material, shouldBuy) -> do
    affordable <- canAfford blueprint material
    -- must afford the robot to buy
    lift $ constrain $ shouldBuy .=> affordable
    -- subtract the cost of the robot if we buy
    let cost = M.map (* oneIf shouldBuy) $ blueprint.robots M.! material
    modify $ \s -> s {materials = M.unionWith (-) (materials s) cost}
  -- Mine phase
  modify $ \s -> s {materials = M.unionWith (+) (materials s) (income s)}
  -- Robot done phase
  forM_ (zip [Ore ..] buyChoices) $ \(material, hasBought) -> do
    modify $ \s -> s {income = M.insertWith (+) material (oneIf hasBought) (income s)}

run :: RobotM () -> Symbolic RobotState
run robots = execStateT robots initState
  where
    initState =
      RobotState
        { materials = M.fromList (zip [Ore ..] (repeat 0)),
          income = M.fromList (zip [Ore ..] [1, 0, 0, 0])
        }

mkObjective :: Int -> Blueprint -> Symbolic ()
mkObjective numMinutes bp = do
  finalState <- run $ replicateM_ numMinutes (evalMinute bp)
  let numGeodes = finalState.materials M.! Geode
  maximize "goal" numGeodes

optimalGeodes :: Int -> Blueprint -> IO Word8
optimalGeodes numMinutes bp = do
  LexicographicResult res <- optimize Lexicographic (mkObjective numMinutes bp)
  let val = fromJust $ getModelValue "goal" res
  return val

part1 :: [Blueprint] -> IO Int
part1 blueprints = do
  vars <- forM blueprints $ \blueprint -> do
    var <- newEmptyMVar
    forkFinally (optimalGeodes 24 blueprint) $ \case
      Left error -> print error
      Right res -> putMVar var res
    return var
  opts <- mapM readMVar vars
  return $ sum $ zipWith (\bpNum opt -> bpNum * fromIntegral opt) (map number blueprints) opts

part2 :: [Blueprint] -> IO Int
part2 blueprints = do
  vars <- forM (take 3 blueprints) $ \blueprint -> do
    var <- newEmptyMVar
    forkFinally (optimalGeodes 32 blueprint) $ \case
      Left error -> print error
      Right res -> putMVar var res
    return var
  opts <- mapM readMVar vars
  return $ product $ map fromIntegral opts

main :: IO ()
main = do
  inp <- getInput 19
  blueprints <- parseIO pInp "day19.input" inp
  part1 blueprints >>= putAnswer 19 Part1
  part2 blueprints >>= putAnswer 19 Part2
