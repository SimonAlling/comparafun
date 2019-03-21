module Main where

import Criterion.Main (defaultMain, bench, nf)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Safe
import SortTestData (testData)

import Sort
import Util

mainWith :: Int -> IO ()
mainWith n =
  let
    items = take n $ concat $ repeat testData
  in do
    printHECs
    putStrLn $ show $ length items
    defaultMain
      [ bench "sort_par" (nf mergesort items)
      ]

main :: IO ()
main =
  let
    readMaybeInt = readMaybe :: String -> Maybe Int
    printHelp = putStrLn "Usage: stack exec -- comparafun-sort 100000 sort +RTS -H1G -A100M -N2"
  in do
    numberOfClusters <- (readMaybeInt =<<) . headMay <$> getArgs
    maybe printHelp mainWith numberOfClusters
