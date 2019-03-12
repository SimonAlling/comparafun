-- Inspired by http://hackage.haskell.org/package/kmeans

module KMeans
    ( kmeans_seq
    , prop_correct
    , theirkmeans
    ) where

import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.List (sort, minimumBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.KMeans as KM; theirkmeans = KM.kmeans

type Point = (Double, Double)

type Centroid = Point

type Cluster = [Point]

kmeans_seq :: Int -> [Point] -> [Cluster]
kmeans_seq = fmap kmeansClusters . splitIntoMax

kmeansClusters :: [Cluster] -> [Cluster]
kmeansClusters clusters = if done then clusters else kmeansClusters reclustered
  where
    done = clusters == reclustered
    reclustered = recluster clusters

recluster :: [Cluster] -> [Cluster]
recluster clusters = recluster' centroids $ concat clusters
  where
    centroids = map centroidOf clusters

recluster' :: [Centroid] -> [Point] -> [Cluster]
recluster' centroids points = map (map snd) $ groupBy ((==) `on` fst) reclustered
  where
    reclustered = sort $ map (\p -> (closestCentroid p, p)) points
    closestCentroid = closest centroids

splitIntoMax :: Int -> [a] -> [[a]]
splitIntoMax k xs = splitEvery step xs
  where step = (length xs + k - 1) `div` k

splitEvery :: Int -> [a] -> [[a]]
splitEvery step ys
  | null zs' = [zs]
  | otherwise = zs : splitEvery step zs'
  where (zs, zs') = splitAt step ys

closest :: [Point] -> Point -> Point
closest = flip $ minimumBy . comparing . sqDistTo

centroidOf :: Cluster -> Point
centroidOf points = (x, y)
  where
    numOfPoints = fromIntegral $ length points
    x = xTotal / numOfPoints
    y = yTotal / numOfPoints
    xTotal = sum $ map fst $ points
    yTotal = sum $ map snd $ points

-- Wasteful to calculate square root; behavior would be identical.
sqDistTo :: Point -> Point -> Double
sqDistTo (x1, y1) (x2, y2) = (x2-x1)^2 + (y2-y1)^2

-- QuickCheck property
prop_correct :: Int -> [Point] -> Bool
prop_correct k points =
  k <= 0 || map (map toList) (kmeans_seq k points) == theirkmeans k (map toList points)

toList :: (a, a) -> [a]
toList (x, y) = [x, y]
