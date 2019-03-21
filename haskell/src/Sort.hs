module Sort
    ( mergesort
    ) where

import Control.DeepSeq (rnf)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Strategy, NFData, parListChunk, rdeepseq, using)

mergesort :: (Ord a, NFData a) => [a] -> [a]
mergesort xs = if length xs > 20 then mergesort_par xs else mergesort_seq xs

mergesort_seq :: (Ord a, NFData a) => [a] -> [a]
mergesort_seq [] = []
mergesort_seq [x] = [x]
mergesort_seq [x, y] = if x < y then [x, y] else [y, x]
mergesort_seq xs =
  merge sortedLeft sortedRight
  where
    (left, right) = splitAt (length xs `div` 2) xs
    sortedLeft = mergesort_seq left
    sortedRight = mergesort_seq right

mergesort_par :: (Ord a, NFData a) => [a] -> [a]
mergesort_par xs =
  forcedLeft `par`
  forcedRight `pseq`
  merge forcedLeft forcedRight
  where
    (left, right) = splitAt (length xs `div` 2) xs
    sortedLeft = mergesort left
    sortedRight = mergesort right
    forcedLeft = forceList sortedLeft
    forcedRight = forceList sortedRight

forceList :: NFData a => [a] -> [a]
forceList xs = rnf xs `pseq` xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge as [] = as
merge [] bs = bs
merge xs@(a:as) ys@(b:bs)
  | a < b     = a:(merge as ys)
  | otherwise = b:(merge xs bs)
