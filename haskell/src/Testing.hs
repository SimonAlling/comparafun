{-# LANGUAGE FlexibleContexts #-}

module Testing where

import Prelude
import Test.QuickCheck (Arbitrary, Property, Gen, choose, listOf, vectorOf, arbitrary, generate, forAll)
import Data.Metric (Metric, distance)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), ExpectDivergent(..))
import Algorithms.Lloyd.Strategies (Partitions(..))
import qualified Algorithms.Lloyd.Sequential as LloydSeq
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
type K = Int

data Mode = Seq | Par Partitions

genPoints :: Dimensions -> Gen [Point]
genPoints = listOf . genPoint

listFromSeed :: (Integral int, Num a, Distribution Uniform a) => Word64 -> (a, a) -> int -> [a]
listFromSeed seed (lo, hi) n = evalState st $ pureMT seed
  where st = replicateM (fromIntegral n) $ sample $ uniform lo hi

vectorFromSeed :: Seed -> (Double, Double) -> Dimensions -> V.Vector Double
vectorFromSeed = compose3 V.fromList listFromSeed

pointFromSeed :: Seed -> Interval Double -> Dimensions -> Point
pointFromSeed seed lohi dim = Point $ vectorFromSeed seed lohi dim

generateTestData :: Seed -> (Double, Double) -> Dimensions -> Size -> Vector Point
generateTestData seed lohi dim n = V.map (Point . (flip (flip vectorFromSeed lohi) dim) . fromIntegral) (V.fromList [1..n])

-- QuickCheck property
prop_correct :: Dimensions -> K -> Property
prop_correct dimensions k = forAll (genPoints dimensions) (flip correctFor k)

point1, point2, point3, point4 :: Point
point1 = Point $ V.fromList [ 1, 2, 3 ]
point2 = Point $ V.fromList [ 1, 1, 5 ]
point3 = Point $ V.fromList [ 5, 8, 2 ]
point4 = Point $ V.fromList [ 5, 8, 9 ]

examplePoints :: [Point]
examplePoints = [point1, point2, point3, point4]

runKmeans :: Mode -> Vector Point -> K -> Vector (Vector Point)
runKmeans mode points k = kmeans expectDivergent metric points initial
  where
    kmeans = case mode of
      Seq -> LloydSeq.kmeans
      Par p -> ($ p) . flip . LloydPar.kmeans
    initial :: Vector Cluster
    initial =
      if V.length points < k
      then error $ show (V.length points) ++ " points, but k = " ++ show k
      else V.zipWith (flip Cluster) points (V.fromList [0 .. k-1])
    expectDivergent :: ExpectDivergent
    expectDivergent = ExpectDivergent 10
    metric :: Vector Double -> Vector Double
    metric = id

correctFor :: [Point] -> K -> Bool
correctFor points k = or [ k <= 0, length points < k, resultSeq == resultPar ]
  where
    pointsV = V.fromList points
    resultSeq = runKmeans  Seq                 pointsV k
    resultPar = runKmeans (Par $ Partitions 4) pointsV k -- arbitrary number of partitions
