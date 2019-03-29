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

deriving instance Generic (Point)
deriving instance NFData (Point)

type Parameters = (Partitions, Seed, Size, K)

mainWith :: Parameters -> IO ()
mainWith (partitions, seed, n, k) =
  let
    interval = (0, 100)
    dimensions = 3
    testData = generateTestData seed interval dimensions n
  in do
    printHECs
    defaultMain
      [ bench "kmeans_seq" (nf (flip (runKmeans  Seq            ) k) testData)
      , bench "kmeans_par" (nf (flip (runKmeans (Par partitions)) k) testData)
      ]

main :: IO ()
main =
  let
    printHelp = putStrLn "Usage: stack exec -- comparafun-kmeans partitions seed n k kmeans +RTS -H1G -A100M -N2"
  in
    parseArgs <$> getArgs >>= maybe printHelp mainWith

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

parseArgs :: [String] -> Maybe Parameters
parseArgs (s_parts : s_seed : s_n : s_k : _) = do
  parts <- Partitions <$> readMaybeInt s_parts
  seed <- fromIntegral <$> readMaybeInt s_seed
  n <- readMaybeInt s_n
  k <- readMaybeInt s_k
  pure (parts, seed, n, k)
parseArgs _ = Nothing
