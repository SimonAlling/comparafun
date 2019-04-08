{-# LANGUAGE RebindableSyntax #-}

module KMeansTesting where

import Prelude hiding ((>>))
import Data.List (intersperse)
import KMeansStuff (K, runKmeans)
import Testing (Parallelism(..), SuiteConfig(..), Seed, Size, Threads)
import Algorithms.Lloyd.Strategies (Partitions(..))
import Util (compose2)

type KMeansParameters = (Parallelism Int, Seed, Size, K)

type PartitionsCalculator = Size -> Threads -> Int

data KMeansConfiguration = KMeansConfiguration
  { ns :: [Int]
  , ks :: [Int]
  , seeds :: [Seed]
  , parts :: [PartitionsCalculator]
  }

instance SuiteConfig KMeansConfiguration where
  problemSizes = ns

defaultKMeansConfig :: KMeansConfiguration
defaultKMeansConfig = KMeansConfiguration
  { ns = [10000, 30000]
  , ks = [2, 10, 50]
  , seeds = [1..3]
  , parts = map (const . const) [ 20, 100, 500 ]
  }

printKMeansBenchmarkInfo :: KMeansParameters -> IO ()
printKMeansBenchmarkInfo (parallelism, seed, n, k) =
  putStrLn $ unlines $ do
    "n = " ++ show n
    "k = " ++ show k
    "seed = " ++ show seed
    "partitions = " ++ show parallelism
    [] where (>>) = (:)

showKMeansConfiguration :: KMeansConfiguration -> String
showKMeansConfiguration ps = unlines $ do
  "********* Benchmark suite *********"
  "  n:           " ++ showItems (ns ps)
  "  k:           " ++ showItems (ks ps)
  "  seed:        " ++ showItems (seeds ps)
  "***********************************"
  [] where (>>) = (:)

showItems :: Show a => [a] -> String
showItems = concat . intersperse ", " . map show
