{-# LANGUAGE OverloadedStrings #-}

module SantaLib
  ( fetchInput,
    fetchDescription,
    getInput,
    getExample,
    submitAnswer,
    putAnswer,
    readText,
    connected,
    toVector,
    (|>),
    filterNums,
    module Advent.Types,
  )
where

import Advent
import Advent.Types
import Data.Foldable
import Data.Function ((&), on)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.HTML.TagSoup
import Data.Char
import Data.List

getOpts :: IO AoCOpts
getOpts = do
  env <- readFile ".env"
  let ls = lines env
  let (year, key) = (read $ head ls, ls !! 1)
  return $ defaultAoCOpts year key

fetchInput :: Integer -> IO ()
fetchInput d = do
  opts <- getOpts
  inp <- runAoC_ opts $ AoCInput (mkDay_ d)
  TIO.writeFile ("input/day" <> show d <> ".input") inp

fetchDescription :: Integer -> IO ()
fetchDescription d = do
  opts <- getOpts
  m <- runAoC_ opts $ AoCPrompt (mkDay_ d)
  TIO.writeFile ("descr/day" <> show d <> "-part1.html") (fromMaybe "Part 1 not unlocked yet" (m !? Part1))
  TIO.writeFile ("descr/day" <> show d <> "-part2.html") (fromMaybe "Part 2 not unlocked yet" (m !? Part2))
  -- hopefully get parse the example. It is usually the first thing within <pre><code> tags.
  let example = pExample (m ! Part1)
  TIO.writeFile ("input/day" <> show d <> "-example.input") example
  return ()

pExample :: Text -> Text
pExample html = fromTagText $ head goal
  where
    tags = parseTags html
    beginsWithPre = partitions (isTagOpenName "pre") tags
    followedByCode = partitions (isTagOpenName "code") <$> beginsWithPre
    inners = head . head $ followedByCode
    goal = filter isTagText inners

getExample :: Int -> IO String
getExample n = readFile ("input/day" <> show n <> "-example.input")

getInput :: Int -> IO String
getInput n = readFile ("input/day" <> show n <> ".input")

submitAnswer :: Integer -> Part -> IO ()
submitAnswer day part = do
  opts <- getOpts
  fp <- case part of
    Part1 -> return ("answer/day" <> show day <> "-part1")
    Part2 -> return ("answer/day" <> show day <> "-part2")
  ans <- readFile fp
  (response, result) <- runAoC_ opts (AoCSubmit (mkDay_ day) part ans)
  TIO.putStrLn response
  print result

putAnswer :: Show a => Integer -> Part -> a -> IO ()
putAnswer day part = writeFile fp . show
  where
    fp = case part of
      Part1 -> "answer/day" <> show day <> "-part1"
      Part2 -> "answer/day" <> show day <> "-part2"

-- Common, useful algorithms:
readText :: Read a => Text -> a
readText = read . T.unpack

-- | Find connected subgraph given initial point and function handling neighbors
-- using breadth first search
connected :: forall f point. (Foldable f, Ord point) => (point -> f point) -> point -> [point]
connected neighbors initPoint = go neighbors (V.singleton initPoint) S.empty
  where
    go :: (point -> f point) -> Vector point -> Set point -> [point]
    go neighbors queue visited
      | null queue = toList visited
      | otherwise = go neighbors (rest <> unvisitedNeighbors) (S.insert current visited)
      where
        current = V.head queue
        rest = V.tail queue
        unvisitedNeighbors = current |> neighbors |> toVector |> V.filter (\p -> not (S.member p visited))

toVector :: (Foldable f) => f a -> Vector a
toVector = V.fromList . toList

(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>

filterNums :: String -> [Int]
filterNums = map read . filter (all isNumber) . groupBy ((==) `on` isNumber)