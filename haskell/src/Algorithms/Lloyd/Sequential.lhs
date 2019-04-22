> {-# LANGUAGE ViewPatterns #-}

A sequential implementation of Lloyd's algorithm for k-means clustering,
adapted from Marlow's _Parallel and Concurrent Programming in Haskell_:

> module Algorithms.Lloyd.Sequential (
>   Point(..),
>   Cluster(..), 
>   ExpectDivergent(..),
>   kmeans,
>   PointSum(..),
>   makeNewClusters,
>   assign,
>   assignPS,
>   step
> )where
> 
> import Prelude hiding (zipWith, map, foldr, replicate, length, zip, head)
> import Control.Applicative ((<$>))
> import Control.Monad (guard)
> import Data.Foldable (Foldable(foldr))
> import Data.Function (on)
> import Data.Functor.Extras ((..:),(...:))
> import Data.Metric (Metric(..))
> import Data.Semigroup (Semigroup(..))
> import Data.Vector (Vector(..), toList, fromList, create, zip, zipWith, map, replicate, minimumBy, length, head, forM_)
> import qualified Data.Vector.Mutable as MV (replicate, read, write)
 
A data point is represented by an n-dimensional real vector:

> data Point = Point 
>   { point :: Vector Double
>   } deriving (Eq, Show)

Point isn't parametrised, so we can't define a functor (or any functor-family)
instances. Instead, we define a specialised map, `pmap`:

> pmap :: (Vector Double -> Vector Double) -> Point -> Point
> pmap f = Point . f . point

To define distance between data-points, we admit arbitrary metric spaces
over real vectors; **However:** it should be noted that convergence is
only guaranteed in the case when the mean of the metric optimises the
same criterion as minimum-distance assignment (`assign` below.) For this
reason, Euclidean-distance is popularly used for mean assignment.

> useMetric :: Metric a => (Vector Double -> a) -> Point -> Point -> Double
> useMetric metric = distance `on` (metric . point)
 
`Point`s may be combined using simple vector addition:

> instance Semigroup Point where
>   (<>) = Point ..: zipWith (+) `on` point

Clusters are represented by an identifier and a centroid:

> data Cluster = Cluster
>   { identifier :: Int
>   , centroid   :: Point
>   } deriving (Show)

Clusters are treated as equivalent if they share a centroid:

> instance Eq Cluster where
>   (==) = (==) `on` centroid

`PointSum` is a point sum; this is an intermediate type used to contain the sum
of points in a set when constructing new clusters. It consists of a count of
included points, as well as the vector sum of the constituent points:

> data PointSum = PointSum Int Point deriving (Show)

Two `PointSum`s can be combined by summing corresponding values:

> instance Semigroup PointSum where
>   PointSum c0 p0 <> PointSum c1 p1 = PointSum (c0+c1) (p0<>p1)

Without dependent types, we can't define a valid Monoid instance for PointSum,
as the identity varies with the dimensions of the point vector. But we can
construct one at runtime:

> emptyPointSum :: Int -> PointSum
> emptyPointSum length = PointSum 0 . Point $ replicate length 0

A point sum can be turned into a cluster by computing its centroid, the average
of all points associated with the cluster:

> toCluster :: Int -> PointSum -> Cluster
> toCluster cid (PointSum count point) = Cluster
>   { identifier = cid
>   , centroid   = map (//count) `pmap` point
>   }
>
> (//) :: Double -> Int -> Double
> x // y = x / fromIntegral y
         
After each iteration, points are associated with the cluster having the nearest
centroid:

> closestCluster :: Metric a => (Vector Double -> a) -> Vector Cluster -> Point -> Cluster
> closestCluster (useMetric -> d) clusters point = fst . minimumBy (compare `on` snd) $ do
>   cluster <- clusters
>   return (cluster, point `d` centroid cluster)
>
> assign :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector Point -> Vector (Vector Point)
> assign metric clusters points = map fromList $ create $ do
>   vector <- MV.replicate (length clusters) []
>   points `forM_` \point -> do
>     let cluster  = closestCluster metric clusters point
>         position = identifier cluster
>     points' <- MV.read vector position
>     MV.write vector position $ point : points'
>   return vector
>
> assignPS :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector Point -> Vector PointSum
> assignPS metric clusters points = reduce <$> assign metric clusters points
>   where reduce  = foldr (<>) (emptyPointSum length') . fmap (PointSum 1)
>         length' = length . point $ head points
>
> makeNewClusters :: Vector PointSum -> Vector Cluster
> makeNewClusters vector = do
>   (pointSum, index) <- zip vector $ fromList [0..length vector]
>   return $ toCluster index pointSum
>
> step :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector Point -> Vector Cluster
> step = makeNewClusters ...: assignPS

The algorithm consists of iteratively finding the centroid of each existing
cluster, then reallocating points according to which centroid is closest, until
convergence. As the algorithm isn't guaranteed to converge, we cut execution if
convergence hasn't been observed some provided number of iterations:

> newtype ExpectDivergent = ExpectDivergent { expectDivergent :: Int }
>
> computeClusters :: Metric a => ExpectDivergent -> (Vector Double -> a) -> Vector Point -> Vector Cluster -> Vector Cluster
> computeClusters (expectDivergent -> expectDivergent) metric = computeClusters' expectDivergent metric 0 
>
> computeClusters' :: Metric a => Int -> (Vector Double -> a) -> Int -> Vector Point -> Vector Cluster -> Vector Cluster
> computeClusters' expectDivergent metric iterations points clusters 
>   | iterations >= expectDivergent = clusters
>   | clusters' == clusters         = clusters 
>   | otherwise                     = computeClusters' expectDivergent metric (succ iterations) points clusters'
>   where clusters' = step metric clusters points
>
> kmeans :: Metric a => ExpectDivergent -> (Vector Double -> a) -> Vector Point -> Vector Cluster -> Vector (Vector Point)
> kmeans expectDivergent metric points initial = assign metric clusters points
>   where clusters = computeClusters expectDivergent metric points initial

A note on initialisation: typically clusters are randomly assigned to data
points prior to the first update step.
