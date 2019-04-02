{-# LANGUAGE ViewPatterns #-}

module Algorithms.Lloyd.Sequential (
  Point(..),
  Cluster(..),
  ExpectDivergent(..),
  kmeans,
  PointSum(..),
  makeNewClusters,
  assign,
  assignPS,
  step
)where

import Prelude hiding (zipWith, map, foldr, replicate, length, zip, head)
import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Foldable (Foldable(foldr))
import Data.Function (on)
import Data.Functor.Extras ((..:),(...:))
import Data.Metric (Metric(..))
import Data.Semigroup (Semigroup(..))
import Data.Vector (Vector(..), toList, fromList, create, zip, zipWith, map, replicate, minimumBy, length, head, forM_)
import qualified Data.Vector.Mutable as MV (replicate, read, write)

data Point = Point
  { point :: Vector Double
  } deriving (Eq, Show)

pmap :: (Vector Double -> Vector Double) -> Point -> Point
pmap f = Point . f . point

useMetric :: Metric a => (Vector Double -> a) -> Point -> Point -> Double
useMetric metric = distance `on` (metric . point)

instance Semigroup Point where
  (<>) = Point ..: zipWith (+) `on` point

data Cluster = Cluster
  { identifier :: Int
  , centroid   :: Point
  } deriving (Show)

instance Eq Cluster where
  (==) = (==) `on` centroid

data PointSum = PointSum Int Point deriving (Show)

instance Semigroup PointSum where
  PointSum c0 p0 <> PointSum c1 p1 = PointSum (c0+c1) (p0<>p1)

emptyPointSum :: Int -> PointSum
emptyPointSum length = PointSum 0 . Point $ replicate length 0

toCluster :: Int -> PointSum -> Cluster
toCluster cid (PointSum count point) = Cluster
  { identifier = cid
  , centroid   = map (//count) `pmap` point
  }

(//) :: Double -> Int -> Double
x // y = x / fromIntegral y

closestCluster :: Metric a => (Vector Double -> a) -> Vector Cluster -> Point -> Cluster
closestCluster (useMetric -> d) clusters point = fst . minimumBy (compare `on` snd) $ do
  cluster <- clusters
  return (cluster, point `d` centroid cluster)

-- Type system can't handle `map fromList . create` here; ($) is magical according to John Hughes.
assign :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector Point -> Vector (Vector Point)
assign metric clusters points = map fromList $ create $ do
  vector <- MV.replicate (length clusters) []
  points `forM_` \point -> do
    let cluster  = closestCluster metric clusters point
        position = identifier cluster
    points' <- MV.read vector position
    MV.write vector position $ point : points'
  return vector

assignPS :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector Point -> Vector PointSum
assignPS metric clusters points = reduce <$> assign metric clusters points
  where reduce  = foldr (<>) (emptyPointSum length') . fmap (PointSum 1)
        length' = length . point $ head points

makeNewClusters :: Vector PointSum -> Vector Cluster
makeNewClusters vector = do
  (pointSum, index) <- zip vector $ fromList [0..length vector]
  return $ toCluster index pointSum

step :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector Point -> Vector Cluster
step = makeNewClusters ...: assignPS

newtype ExpectDivergent = ExpectDivergent { expectDivergent :: Int }

computeClusters :: Metric a => ExpectDivergent -> (Vector Double -> a) -> Vector Point -> Vector Cluster -> Vector Cluster
computeClusters (expectDivergent -> expectDivergent) metric = computeClusters' expectDivergent metric 0

computeClusters' :: Metric a => Int -> (Vector Double -> a) -> Int -> Vector Point -> Vector Cluster -> Vector Cluster
computeClusters' expectDivergent metric iterations points clusters
  | iterations >= expectDivergent = clusters
  | clusters' == clusters         = clusters
  | otherwise                     = computeClusters' expectDivergent metric (succ iterations) points clusters'
  where clusters' = step metric clusters points

kmeans :: Metric a => ExpectDivergent -> (Vector Double -> a) -> Vector Point -> Vector Cluster -> Vector (Vector Point)
kmeans expectDivergent metric points initial = assign metric clusters points
  where clusters = computeClusters expectDivergent metric points initial
