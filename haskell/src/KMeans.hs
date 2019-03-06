{-# LANGUAGE MagicHash #-}

module KMeans
    ( kmeans_seq
    , kmeans_par
    ) where

import GHC.Exts -- Int# etc
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)

kmeans_seq :: Int -> [[Double]] -> [[[Double]]]
kmeans_seq k xs = []

kmeans_par :: Int -> [[Double]] -> [[[Double]]]
kmeans_par k xs = []
