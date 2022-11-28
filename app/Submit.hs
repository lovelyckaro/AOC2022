module Main where
import SantaLib
import System.Environment
import System.Exit (exitFailure)

printUsage :: IO a
printUsage = putStrLn "To submit solution for day n part p:\ncabal run submit n p" >> exitFailure

main :: IO ()
main = do
  env <- getArgs
  (day, partInt) <- case env of
    [dayStr, partString] -> return (read @Integer dayStr, read @Int partString)
    _ -> printUsage
  submitAnswer day (toEnum $ partInt - 1)