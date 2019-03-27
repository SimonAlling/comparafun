{-# LANGUAGE FlexibleContexts #-}

module Testing where

import Prelude
import Test.QuickCheck (Arbitrary, Property, Gen, choose, listOf, vectorOf, arbitrary, generate, forAll)
import Data.Metric (Metric, distance)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), ExpectDivergent(..), kmeans)
import qualified Algorithms.Lloyd.Strategies as LloydPar
import Data.Random.Source.PureMT
import Data.Random (Uniform, Distribution, uniform, sample)
import Control.Monad.State (evalState, replicateM)
import GHC.Word (Word64)
import Util (compose2, compose3)
import KMeansStuff (Dimensions(..), squareDistance, genPoint)

type Seed = Word64
type Size = Int
type Interval a = (a, a)

seed = 42

genPoints :: Dimensions -> Gen [Point]
genPoints = listOf . genPoint

listFromSeed :: (Integral int, Num a, Distribution Uniform a) => Word64 -> (a, a) -> int -> [a]
listFromSeed seed (lo, hi) n = evalState st $ pureMT seed
  where st = replicateM (fromIntegral n) $ sample $ uniform lo hi

vectorFromSeed :: Seed -> (Double, Double) -> Dimensions -> V.Vector Double
vectorFromSeed = compose3 V.fromList listFromSeed

pointFromSeed :: Seed -> Interval Double -> Dimensions -> Point
pointFromSeed seed lohi dim = Point $ vectorFromSeed seed lohi dim

exampleData :: Seed -> (Double, Double) -> Dimensions -> Size -> [Point]
exampleData seed lohi dim n = map (Point . (flip (flip vectorFromSeed lohi) dim) . fromIntegral) [1..n]

-- QuickCheck property
prop_correct :: Dimensions -> Int -> Property
prop_correct dimensions k = forAll (genPoints dimensions) (flip correctFor k)

correctFor :: [Point] -> Int -> Bool
correctFor points k = k <= 1 || length points <= k || resultSeq == resultPar
  where
    resultSeq =          kmeans expectDivergent metric        pointsV initial
    resultPar = LloydPar.kmeans expectDivergent metric chunks pointsV initial
    expectDivergent = ExpectDivergent 10
    metric = id
    chunks = LloydPar.Partitions 4
    pointsV :: Vector Point
    pointsV = V.fromList $ points
    initial :: Vector Cluster
    initial = let numOfPoints = length points in
      if numOfPoints < k
      then error $ show numOfPoints ++ " points, but k = " ++ show k
      else V.fromListN k $ zipWith (flip Cluster) points [1..]
