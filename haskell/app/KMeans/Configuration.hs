{-# LANGUAGE RebindableSyntax #-}

module Configuration where

import Prelude hiding ((>>))
import Testing (Seed, Size, K, generateTestData, runKmeans, Mode(..))
import Algorithms.Lloyd.Strategies (Partitions(..))

type Parameters = (Partitions, Seed, Size, K)

printInfo :: Parameters -> IO ()
printInfo (Partitions p, seed, n, k) =
  putStrLn $ unlines $ do
    "n = " ++ show n
    "k = " ++ show k
    "seed = " ++ show seed
    "partitions = " ++ show p
    []
    where (>>) = (:)
