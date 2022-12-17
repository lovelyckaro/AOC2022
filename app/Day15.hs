{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (forM_)
import Data.Foldable (maximumBy)
import Data.Maybe (fromJust)
import Data.SBV
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing

data Sensor = Sensor {x, y, beaconX, beaconY :: Integer}
  deriving (Show)

pSensor :: Parser Sensor
pSensor = do
  mapM_ symbol ["Sensor", "at", "x", "="]
  x <- lexeme (signed decimal)
  mapM_ symbol [",", "y", "="]
  y <- lexeme (signed decimal)
  mapM_ symbol [":", "closest", "beacon", "is", "at", "x", "="]
  beaconX <- lexeme (signed decimal)
  mapM_ symbol [",", "y", "="]
  beaconY <- lexeme (signed decimal)
  return $ Sensor x y beaconX beaconY

pInp :: Parser [Sensor]
pInp = some (lexemeLn pSensor)

manDist :: Num a => (a, a) -> (a, a) -> a
manDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

sensorXs :: Integer -> Sensor -> Set Integer
sensorXs y sensor = S.fromList xs
  where
    dist = manDist (sensor.x, sensor.y) (sensor.beaconX, sensor.beaconY)
    xMax = sensor.x + dist
    xMin = sensor.x - dist
    yDiff = abs (sensor.y - y)
    xs = [xMin + yDiff .. xMax - yDiff]

beacons :: [Sensor] -> Integer -> Set Integer
beacons sensors y = S.fromList $ map beaconX $ filter ((== y) . beaconY) sensors

part1 :: Integer -> [Sensor] -> Int
part1 y sensors = S.size . (`S.difference` beacons sensors y) . foldr (S.union . sensorXs y) S.empty $ sensors

part2 :: [Sensor] -> Integer -> Symbolic ()
part2 sensors maxVal = do
  x <- sbvExists "x"
  y <- sbvExists "y"
  let max = literal maxVal
  constrain $ x .>= 0
  constrain $ y .>= 0
  constrain $ x .<= max
  constrain $ y .<= max
  forM_ sensors $ \s -> do
    let sensorX = literal s.x
    let sensorY = literal s.y
    let dist = literal $ manDist (s.x, s.y) (s.beaconX, s.beaconY)
    constrain $ manDist (x, y) (sensorX, sensorY) .> dist
  sObserve "res" $ x * 4000000 + y

main :: IO ()
main = do
  inp <- getInput 15
  sensors <- parseIO pInp "day15.input" inp
  putAnswer 15 Part1 $ part1 2000000 sensors
  part2sol <- sat $ part2 sensors 4000000
  let val :: Integer = fromJust $ getModelValue "res" part2sol
  putAnswer 15 Part2 val
