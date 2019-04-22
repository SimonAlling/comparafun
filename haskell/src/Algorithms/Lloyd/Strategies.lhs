> {-# LANGUAGE ViewPatterns #-}

A parallel implementation of Lloyd's algorithm for k-means clustering,
adapted from Marlow's _Parallel and Concurrent Programming in Haskell_.
Here we use Evaluation Strategies to parallelise the assignment of
points to clusters:

> module Algorithms.Lloyd.Strategies (
>   Point(..),
>   Cluster(..), 
>   ExpectDivergent(..),
>   Partitions(..),
>   kmeans,
>   step
> ) where
>
> import Prelude hiding (zipWith, foldr1, map)
> import Control.Parallel.Strategies (Strategy(..), parTraversable, using, rseq)
> import Data.Foldable (Foldable(foldr1))
> import Data.Functor.Extras ((..:))
> import Data.Metric (Metric(..))
> import Data.Semigroup (Semigroup(..))
> import Data.Vector (Vector(..), zipWith, map)
> import Data.Vector.Split (chunkInto)
> import Algorithms.Lloyd.Sequential (Cluster(..), Point(..), ExpectDivergent(..), PointSum(..), makeNewClusters, assignPS, assign)

We can combine two vectors of some same type $t$ provided we know how to
combine two $t$s:

> (<><>) :: Semigroup a => Vector a -> Vector a -> Vector a
> (<><>) = zipWith (<>)

(We can't make $Vector a$ an instance of $Semigroup$, because it already is. But
that instance has $(<>) = (++)$, so we can't use it here.)

Step is modified to, given a partitioned list of points, perform
classification in parallel:

> step :: Metric a => (Vector Double -> a) -> Vector Cluster -> Vector (Vector Point) -> Vector Cluster
> step = makeNewClusters . foldr1 (<><>) . with (parTraversable rseq) ..: fmap ..: assignPS
>
> with :: Strategy a -> a -> a
> with = flip using

This version of k-means takes an additional arguments -- the number of
partitions the set of points'll be divided into. This needn't equal the
number of processors: if there are more spark than cores, the runtime
can be trusted to schedule unallocated sparks so soon as a core becomes
available. That said: if there are too many work items, the overhead of
recombination may exceed the speed-up provided by parallellism; if there
are too few items, and those items vary in cost, some of our cores may
be unused for part of the computation.

> newtype Partitions = Partitions { partitions :: Int }
>
> computeClusters :: Metric a => ExpectDivergent -> (Vector Double -> a) -> Partitions -> Vector Point -> Vector Cluster -> Vector Cluster
> computeClusters (expectDivergent -> expectDivergent) metric = computeClusters' expectDivergent metric 0  ..: chunkInto . partitions
>
> computeClusters' :: Metric a => Int -> (Vector Double -> a) -> Int -> Vector (Vector Point) -> Vector Cluster -> Vector Cluster
> computeClusters' expectDivergent metric iterations points clusters 
>   | iterations >= expectDivergent = clusters
>   | clusters' == clusters         = clusters 
>   | otherwise                     = computeClusters' expectDivergent metric (succ iterations) points clusters'
>   where clusters' = step metric clusters points
>
> kmeans :: Metric a => ExpectDivergent -> (Vector Double -> a) -> Partitions -> Vector Point -> Vector Cluster -> Vector (Vector Point)
> kmeans expectDivergent metric chunks points initial = assign metric clusters points
>   where clusters = computeClusters expectDivergent metric chunks points initial
