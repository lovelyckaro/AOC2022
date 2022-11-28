{-# LANGUAGE TypeApplications #-}
module Main where
import SantaLib
import System.Environment

main :: IO ()
main = do
  env <- getArgs
  (day, partInt) <- case env of
    [dayStr, partString] -> return (read @Integer dayStr, read @Int partString)
    _ -> error "Usage cabal run submit [day in 1..25] [part in 1..2]" 
  submitAnswer day (toEnum $ partInt - 1)