{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module KMeansStuff where

import Data.Metric (Metric(..))
import Data.Vector (Vector(..))
import qualified Data.Vector as V
import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), ExpectDivergent(..))
import Algorithms.Lloyd.Strategies (Partitions(..))
import qualified Algorithms.Lloyd.Sequential as LloydSeq
import qualified Algorithms.Lloyd.Strategies as LloydPar
import Data.Function (on)
import Testing (Parallelism(..), Seed, Size, Interval, Dimensions, vectorFromSeed)
import Util ((<$$>))
import Data.Random (Uniform, Distribution, uniform, sample)

type K = Int

instance Metric (Vector Double) where
  distance = squareDistance

squareDistance :: Num a => Vector a -> Vector a -> a
squareDistance = V.sum <$$> V.zipWith ((^2) <$$> (-))

pointFromSeed :: (Real a, Distribution Uniform a) => Seed -> Interval a -> Dimensions -> Point
pointFromSeed seed lohi dim = Point $ fmap realToFrac $ vectorFromSeed seed lohi dim

generateKMeansData :: (Real a, Distribution Uniform a) => Seed -> (a, a) -> Dimensions -> Size -> Vector Point
generateKMeansData seed lohi dim n = V.map (Point . fmap realToFrac . (flip (flip vectorFromSeed lohi) dim) . fromIntegral) (V.fromList [1..n])

runKmeans :: Parallelism Int -> Vector Point -> K -> Vector (Vector Point)
runKmeans parallelism points k = kmeans expectDivergent metric points initial
  where
    kmeans = case parallelism of
      Sequential -> LloydSeq.kmeans
      Parallel p -> ($ Partitions p) <$$> LloydPar.kmeans
    initial :: Vector Cluster
    initial =
      if V.length points < k
      then error $ show (V.length points) ++ " points, but k = " ++ show k
      else V.zipWith (flip Cluster) points (V.fromList [0 .. k-1])
    expectDivergent :: ExpectDivergent
    expectDivergent = ExpectDivergent 100000
    metric :: Vector Double -> Vector Double
    metric = id
