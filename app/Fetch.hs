{-# LANGUAGE TypeApplications #-}
module Main where
import SantaLib
import System.Environment

main :: IO ()
main = do
  env <- getArgs
  day <- case env of
    [dayStr] -> return $ read @Integer dayStr
    _ -> error "No day submitted"
  putStrLn $ "Fetching day " <> show day <> "..."
  putStrLn "Fetching Description"
  fetchDescription day
  putStrLn "Fetching input"
  fetchInput day
  putStrLn "Done!"