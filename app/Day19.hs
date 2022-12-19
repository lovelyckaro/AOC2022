{-# LANGUAGE OverloadedRecordDot #-}

module Main where

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

evalMinute :: Blueprint -> RobotM ()
evalMinute blueprint = do
  -- Buy phase
  buyChoices <- lift $ sBools ["buyOreRobot", "buyClayRobot", "buyObsidianRobot", "buyGeodeRobot"]
  -- Can buy at most one robot
  lift $ constrain $ (.<= (1 :: SWord8)) $ sum $ map oneIf buyChoices
  forM_ (zip [Ore ..] buyChoices) $ \(material, shouldBuy) -> do
    affordable <- canAfford blueprint material
    -- must afford the robot to buy
    lift $ constrain $ shouldBuy .=> affordable
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
  finalResources <- sWord8s ["Final ore", "Final clay", "Final obsidian", "Final geode"]
  forM_ (zip [Ore ..] finalResources) $ \(resource, var) ->
    constrain $ var .== finalState.materials M.! resource
  finalIncome <- sWord8s ["Final ore robots", "Final clay robots", "Final obsidian robots", "Final geode robots"]
  forM_ (zip [Ore ..] finalIncome) $ \(resource, var) ->
    constrain $ var .== finalState.income M.! resource

optimalGeodes :: Int -> Blueprint -> IO Word8
optimalGeodes numMinutes bp = do
  putStrLn $ "Starting optimize of blueprint number: " <> show bp.number
  LexicographicResult res <- optimize Lexicographic (mkObjective numMinutes bp)
  let val = fromJust $ getModelValue "goal" res
  putStrLn $ "Blueprint number: " <> show bp.number <> ", optimal: " <> show val
  return val

part1 :: [Blueprint] -> IO Int
part1 blueprints = do
  opts <- mapM (optimalGeodes 24) blueprints
  return $ sum $ zipWith (\bpNum opt -> bpNum * fromIntegral opt) (map number blueprints) opts

part2 :: [Blueprint] -> IO Int
part2 blueprints = do
  opts <- mapM (optimalGeodes 32) (take 3 blueprints)
  return $ product $ map fromIntegral opts

main :: IO ()
main = do
  inp <- getInput 19
  blueprints <- parseIO pInp "day19.input" inp
  part1 blueprints >>= putAnswer 19 Part1
  part2 blueprints >>= putAnswer 19 Part2
