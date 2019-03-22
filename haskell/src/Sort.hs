{-# LANGUAGE ScopedTypeVariables, MagicHash, BangPatterns #-}
-- {-# LANGUAGE Trustworthy #-}

module Sort
    ( mergesort_par
    , mergesort_seq
    ) where

import Control.DeepSeq (rnf)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Strategy, NFData, parListChunk, rdeepseq, using)
--
-- mergesort :: (Ord a, NFData a) => [a] -> [a]
-- mergesort xs = if length xs > 100 then mergesort_par xs else mergesort_seq xs
--
-- mergesort_seq :: (Ord a, NFData a) => [a] -> [a]
-- mergesort_seq [] = []
-- mergesort_seq [x] = [x]
-- mergesort_seq [x, y] = if x < y then [x, y] else [y, x]
-- mergesort_seq xs =
--   merge sortedLeft sortedRight
--   where
--     (left, right) = splitAt (length xs `div` 2) xs
--     sortedLeft = mergesort_seq left
--     sortedRight = mergesort_seq right
--
-- mergesort_par :: (Ord a, NFData a) => [a] -> [a]
-- mergesort_par xs =
--   forcedLeft `par`
--   forcedRight `pseq`
--   merge forcedLeft forcedRight
--   where
--     (left, right) = splitAt (length xs `div` 2) xs
--     sortedLeft = mergesort left
--     sortedRight = mergesort right
--     forcedLeft = forceList sortedLeft
--     forcedRight = forceList sortedRight
--
-- forceList :: NFData a => [a] -> [a]
-- forceList xs = rdeepseq xs `pseq` xs
--
-- merge :: (Ord a) => [a] -> [a] -> [a]
-- merge as [] = as
-- merge [] bs = bs
-- merge xs@(a:as) ys@(b:bs)
--   | a < b     = a:(merge as ys)
--   | otherwise = b:(merge xs bs)

mergesort_par :: Ord a => [a] -> [a]
mergesort_par = sortBy_par compare

mergesort_seq :: Ord a => [a] -> [a]
mergesort_seq = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

sortBy_par :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy_par cmp = mergeAll . sequences
  where
    sequences :: [a] -> [[a]]
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as
