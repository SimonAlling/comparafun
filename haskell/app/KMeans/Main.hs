module Main where

import Criterion.Main (defaultMain, bench, nf)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Safe
import KMeansTestData (testData)
import Util

mainWith :: Int -> IO ()
mainWith k =
  let
    exampleData = testData
  in do
    printHECs
    defaultMain
      []

main :: IO ()
main =
  let
    readMaybeInt = readMaybe :: String -> Maybe Int
    printHelp = putStrLn "Usage: stack exec -- comparafun-kmeans 3 kmeans +RTS -H1G -A100M -N2"
  in do
    numberOfClusters <- (readMaybeInt =<<) . headMay <$> getArgs
    maybe printHelp mainWith numberOfClusters
