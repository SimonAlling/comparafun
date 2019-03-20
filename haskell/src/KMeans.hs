-- Inspired by http://hackage.haskell.org/package/kmeans

module KMeans
    ( kmeans_seq
    , theirkmeans
    , centroidOf
    , Point
    , Cluster
    , Dimensions(..)
    ) where

import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.List (sort, minimumBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.KMeans as KM; theirkmeans = KM.kmeans

newtype Dimensions = D Int
  deriving (Eq, Show)

type Point = [Double]

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
centroidOf points = map (/ numOfPoints) sumOfPoints
  where
    numOfPoints :: Num a => a
    numOfPoints = fromIntegral $ length points

    sumOfPoints :: Point
    sumOfPoints = foldr (zipWith (+)) (repeat 0) points

-- Wasteful to calculate square root; behavior would be identical.
sqDistTo :: Point -> Point -> Double
sqDistTo p q = sum $ zipWith (\x y -> (x-y)^2) p q
