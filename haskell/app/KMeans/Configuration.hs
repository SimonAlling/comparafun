{-# LANGUAGE RebindableSyntax #-}

module Configuration where

import Prelude hiding ((>>))
import Data.List (intersperse)
import Testing (Seed, Size, K, generateTestData, runKmeans, Mode(..))
import Algorithms.Lloyd.Strategies (Partitions(..))

type Parameters = (Partitions, Seed, Size, K)

data BenchmarkConfig = BenchmarkConfig
  { ns :: [Int]
  , ks :: [Int]
  , seeds :: [Seed]
  , parts :: [Int]
  }

printInfo :: Parameters -> IO ()
printInfo (Partitions p, seed, n, k) =
  putStrLn $ unlines $ do
    "n = " ++ show n
    "k = " ++ show k
    "seed = " ++ show seed
    "partitions = " ++ show p
    []
    where (>>) = (:)

printBenchmarkConfig :: BenchmarkConfig -> IO ()
printBenchmarkConfig ps = putStrLn $ unlines $ do
  "***** Benchmark configuration *****"
  "  n:           " ++ showItems (ns ps)
  "  k:           " ++ showItems (ks ps)
  "  seed:        " ++ showItems (seeds ps)
  "  partitions:  " ++ showItems (parts ps)
  "***********************************"
  [] where (>>) = (:)

showItems :: Show a => [a] -> String
showItems = concat . intersperse ", " . map show
