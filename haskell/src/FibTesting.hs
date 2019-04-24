{-# LANGUAGE RebindableSyntax #-}

module FibTesting where

import Prelude hiding ((>>))
import Data.List (intersperse)
import Testing (Parallelism(..), SuiteConfig(..), Seed, Size)

type Depth = Int
type Width = Int

type FibParameters = (Parallelism Int, Depth, Width)

data FibConfiguration = FibConfiguration
  { depths :: [Depth]
  , widths :: [Width]
  }

instance SuiteConfig FibConfiguration where
  problemSizes = widths

defaultFibConfig :: FibConfiguration
defaultFibConfig = FibConfiguration
  { depths = [30]
  , widths = [1000]
  }

printFibBenchmarkInfo :: FibParameters -> IO ()
printFibBenchmarkInfo (parallelism, depth, width) =
  putStrLn $ unlines $ do
    "depth = " ++ show depth
    "width = " ++ show width
    [] where (>>) = (:)

showFibConfiguration :: FibConfiguration -> String
showFibConfiguration ps = unlines $ do
  "********* Benchmark suite *********"
  "  depth:       " ++ showItems (depths ps)
  "  width:       " ++ showItems (widths ps)
  "***********************************"
  [] where (>>) = (:)

showItems :: Show a => [a] -> String
showItems = concat . intersperse ", " . map show
