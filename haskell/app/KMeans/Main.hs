{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Criterion.Main (defaultMain, bench, nf)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Safe
import Util
import Testing (Seed, Size, K, generateTestData, runKmeans, Mode(..))
import Algorithms.Lloyd.Sequential (Point(..))
import Algorithms.Lloyd.Strategies (Partitions(..))
import Configuration (Parameters, BenchmarkConfig(..), printInfo, printBenchmarkConfig)

deriving instance Generic (Point)
deriving instance NFData (Point)

data WhichOne = OnlySequential | OnlyParallel | Both
  deriving Eq

data Batch = Batch

-- These are used for parsing the output:
benchmarkDivider, suiteDivider :: String
benchmarkDivider = "-------- BENCHMARK"
suiteDivider = "-------- SUITE"

mainWith :: Parameters -> WhichOne -> IO ()
mainWith params@(partitions, seed, n, k) whichOne =
  let
    interval = (0, 100)
    dimensions = 3
    testData = generateTestData seed interval dimensions n
  in do
    putStrLn benchmarkDivider
    printInfo params
    defaultMain $ map snd $ filter fst $
      [ (,) (whichOne /= OnlyParallel  ) $ bench "kmeans_seq" (nf (flip (runKmeans  Seq            ) k) testData)
      , (,) (whichOne /= OnlySequential) $ bench "kmeans_par" (nf (flip (runKmeans (Par partitions)) k) testData)
      ]

benchmarkConfig :: BenchmarkConfig
benchmarkConfig = BenchmarkConfig
  { ns = [10000, 30000]
  , ks = [2, 10, 50]
  , seeds = [1..3]
  , parts = [20, 100, 500]
  }

batch :: IO ()
batch = do
  printBenchmarkConfig benchmarkConfig
  with (ns benchmarkConfig) as $ \n ->
    with (ks benchmarkConfig) as $ \k ->
      with (seeds benchmarkConfig) as $ \seed -> do
        mainWith (Partitions 1, seed, n, k) OnlySequential
        with (pure $ n `div` 20) as $ \partitions ->
          mainWith (Partitions partitions, seed, n, k) OnlyParallel
  where
    with = const . flip mapM_
    as = ()

main :: IO ()
main =
  let
    printHelp = putStrLn $ unlines
      [ "Usage:"
      , "* stack exec -- comparafun-kmeans partitions seed n k kmeans +RTS -H1G -A100M -N2"
      , "* stack exec -- comparafun-kmeans batch kmeans +RTS -H1G -A100M -N2"
      ]
  in do
    putStrLn suiteDivider
    printCPUInfo
    printHECs
    parseArgs <$> getArgs >>= maybe printHelp (either (flip mainWith Both) $ const batch)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

parseArgs :: [String] -> Maybe (Either Parameters Batch)
parseArgs ("batch" : _) = pure (Right Batch)
parseArgs (s_parts : s_seed : s_n : s_k : _) = do
  parts <- Partitions <$> readMaybeInt s_parts
  seed <- fromIntegral <$> readMaybeInt s_seed
  n <- readMaybeInt s_n
  k <- readMaybeInt s_k
  pure $ Left (parts, seed, n, k)
parseArgs _ = Nothing
