{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Criterion.Main (Benchmark, defaultMain, bench, nf, bgroup)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (NFData)
import Safe
import Util (equality, printSystemInfo)
import Testing (Parallelism(..), SuiteConfig(..), Seed, Size, Threads, Interval, Dimensions)
import KMeansStuff (K, runKmeans, generateKMeansData)
import KMeansTesting (KMeansParameters, KMeansConfiguration(..), PartitionsCalculator, printKMeansBenchmarkInfo, showKMeansConfiguration, defaultKMeansConfig)
import qualified KMeansTesting
import FibTesting (Width, Depth, FibParameters, FibConfiguration(..), printFibBenchmarkInfo, showFibConfiguration, defaultFibConfig)
import Fib (fibonaccis_seq, fibonaccis_par, fibonaccis_parChunk)
import Algorithms.Lloyd.Sequential (Point(..))
import Algorithms.Lloyd.Strategies (Partitions(..))
import qualified Svg as Svg
import Data.Vector ((!))

deriving instance Generic (Point)
deriving instance NFData (Point)

data UserRequest
  = KMeans (Maybe KMeansParameters)
  | KMeansCorrectness KMeansParameters
  | Fib (Maybe FibParameters)

interval :: Interval Int
interval = (0, 100)

dimensions :: Dimensions
dimensions = 2

renderConfig :: Svg.RenderConfig
renderConfig = Svg.defaultRenderConfig { Svg.scalingFactor = 4 }

runKMeansWith :: KMeansParameters -> IO ()
runKMeansWith params@(parallelism, seed, n, k) =
  let
    testData = generateKMeansData seed interval dimensions n
    resultSeq = runKmeans Sequential testData k
    resultPar = runKmeans parallelism testData k
    serialize = show . fmap point
    serializeResult = show . (fmap $ fmap point)
    filename_testdata = KMeansTesting.filename "testdata" dimensions params
    filename_expected = KMeansTesting.filename "haskell" dimensions params
    filename_plot = KMeansTesting.filename "haskell.svg" dimensions params
  in do
    putStrLn $ "Writing kmeans test data to "++filename_testdata
    writeFile filename_testdata (serialize testData)
    printKMeansBenchmarkInfo params
    writeFile filename_expected (serializeResult resultSeq)
    Svg.plotIO filename_plot renderConfig (\(Point p) -> (p ! 0, p ! 1)) resultSeq
    putStrLn $
      if resultSeq == resultPar then "OK"
      else "Parallel and sequential results differ."

plotScalaOutput :: String -> IO ()
plotScalaOutput filename = do
  scalaOutput <- parseScalaOutput <$> readFile filename
  let filename_plot = "kmeans-scala.svg"
  Svg.plotIO filename_plot renderConfig (\p -> (p !! 0, p !! 1)) scalaOutput
  where
    parseScalaOutput :: String -> [[[Double]]]
    parseScalaOutput = read

benchKMeansWith :: KMeansParameters -> IO ()
benchKMeansWith params@(parallelism, seed, n, k) =
  let
    testData = generateKMeansData seed interval dimensions n
  in do
    printKMeansBenchmarkInfo params
    defaultMain $ pure $ bench name $ nf (flip (runKmeans parallelism) k) testData
  where
    name = case parallelism of
      Sequential -> "kmeans_seq"
      Parallel _ -> "kmeans_par"

benchFibWith :: FibParameters -> IO ()
benchFibWith params@(parallelism, depth, width) =
  let
    testData = replicate width depth
  in do
    printFibBenchmarkInfo params
    defaultMain $ pure $ bench name $ nf f testData
  where
    (name, f) = case parallelism of
      Sequential -> ("fib_seq", fibonaccis_seq)
      Parallel chunks -> case chunks of
        0 -> ("fib_parList", fibonaccis_par)
        c -> ("fib_parListChunk", fibonaccis_parChunk c)

createKMeansBatch :: Threads -> KMeansConfiguration -> Benchmark
createKMeansBatch threads config = bgroup "kmeans" $ map withN $ problemSizes config
  where
    withN :: Size -> Benchmark
    withN n = bgroup (equality "n" n) $ map withK $ ks config
      where
        withK :: K -> Benchmark
        withK k = bgroup (equality "k" k) $ concatMap withSeed $ seeds config
          where
            withSeed :: Seed -> [Benchmark]
            withSeed seed = theSeqOne : map withP (parts config)
              where
                testData = generateKMeansData seed interval dimensions n
                theSeqOne :: Benchmark
                theSeqOne = bench (name 0) $ nf (flip (runKmeans Sequential) k) testData
                withP :: PartitionsCalculator -> Benchmark
                withP pCal = bench (name p) $ nf (flip (runKmeans $ Parallel p) k) testData
                  where p = pCal n threads
                name :: Int -> String
                name p = equality "seed" seed ++ "/" ++ equality "par" p

createFibBatch :: Threads -> FibConfiguration -> Benchmark
createFibBatch threads config = bgroup "fib" $ map withWidth $ problemSizes config
  where
    withWidth :: Width -> Benchmark
    withWidth width = bgroup (equality "width" width) $ concatMap withDepth $ depths config
      where
        withDepth :: Depth -> [Benchmark]
        withDepth depth = concat [ if threads > 2 then [] else pure theSeqOne, pure theParChunkOne ]
          where
            testData = replicate width depth
            theSeqOne      = bench "seq"      $ nf (fibonaccis_seq)              testData
            -- theParOne      = bench "par"      $ nf (fibonaccis_par)              testData
            theParChunkOne = bench "parChunk" $ nf (fibonaccis_parChunk threads) testData

runBenchmark :: Maybe String -> Benchmark -> IO ()
runBenchmark info b = do
  maybe (return ()) putStrLn info
  defaultMain (pure b)

main :: IO ()
main =
  let
    printHelp = putStrLn $ unlines
      [ "Usage:"
      , "stack exec -- comparafun kmeans SIZE K SEED seq +RTS -H1G -A100M -N4"
      , "stack exec -- comparafun kmeans SIZE K SEED PARTITIONS +RTS -H1G -A100M -N4"
      , "stack exec -- comparafun kmeans batch +RTS -H1G -A100M -N4"
      , "stack exec -- comparafun kmeans correctness SIZE K SEED PARTITIONS"
      , "stack exec -- comparafun fib DEPTH WIDTH seq +RTS -H1G -A100M -N4"
      , "stack exec -- comparafun fib DEPTH WIDTH CHUNKS +RTS -H1G -A100M -N4"
      , "stack exec -- comparafun fib batch +RTS -H1G -A100M -N4"
      ]
  in do
    printSystemInfo
    t <- getNumCapabilities
    getArgs <&> parseArgs >>= maybe printHelp (processUserRequest t)
  where
    processUserRequest :: Threads -> UserRequest -> IO ()
    processUserRequest t = \case
      KMeans x -> maybe runBatch_kmeans benchKMeansWith x
      KMeansCorrectness x -> runKMeansWith x
      Fib x -> maybe runBatch_fib benchFibWith x
      where
        runBatch_kmeans = runBenchmark info_kmeans $ createKMeansBatch t defaultKMeansConfig
        runBatch_fib    = runBenchmark info_fib    $ createFibBatch    t defaultFibConfig
        info_kmeans = pure $ showKMeansConfiguration defaultKMeansConfig
        info_fib    = pure $ showFibConfiguration    defaultFibConfig

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

readMaybeParallelism :: Read a => String -> Maybe (Parallelism a)
readMaybeParallelism = \case
  "seq" -> pure Sequential -- NOTE: must be string literal for pattern matching
  x     -> Parallel <$> readMaybe x

-- NOTE: String literals must be literals to work with pattern matching.
parseArgs :: [String] -> Maybe UserRequest
parseArgs = \case
  ("kmeans" : "correctness" : s_n : s_k : s_seed : s_p : _) -> do
    n <- readMaybeInt s_n
    k <- readMaybeInt s_k
    seed <- fromIntegral <$> readMaybeInt s_seed
    p <- readMaybeParallelism s_p
    return $ KMeansCorrectness (p, seed, n, k)
  ("kmeans" : "batch" : _) -> pure $ KMeans Nothing
  ("kmeans" : s_n : s_k : s_seed : s_p : _) -> do
    n <- readMaybeInt s_n
    k <- readMaybeInt s_k
    seed <- fromIntegral <$> readMaybeInt s_seed
    p <- readMaybeParallelism s_p
    return $ KMeans $ pure (p, seed, n, k)
  ("fib" : "batch" : _) -> pure $ Fib Nothing
  ("fib" : s_depth : s_width : s_p : _) -> do
    depth <- readMaybeInt s_depth
    width <- readMaybeInt s_width
    p <- readMaybeParallelism s_p
    return $ Fib $ pure (p, depth, width)
  _ -> Nothing
