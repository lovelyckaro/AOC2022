module Main where

import SantaLib
import System.Environment
import System.Exit (exitFailure)

printUsage :: IO a
printUsage = putStrLn "To fetch description, your input, and (hopefully) the example input for day n:\ncabal run fetch n" >> exitFailure

main :: IO ()
main = do
  env <- getArgs
  day <- case env of
    [dayStr] -> return $ read @Integer dayStr
    _ -> printUsage
  putStrLn $ "Fetching day " <> show day <> "..."
  putStrLn "Fetching Description"
  fetchDescription day
  putStrLn "Fetching input"
  fetchInput day
  putStrLn "Done!"
