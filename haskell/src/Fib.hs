{-# LANGUAGE MagicHash #-}

module Fib
    ( fibonaccis_seq
    , fibonaccis_par
    , fibonaccis_parChunk
    ) where

import GHC.Exts -- Int# etc
import Control.Parallel.Strategies (rdeepseq, using, parListChunk, parList)

-- Boxed ("regular") ints:
fib :: Int -> Int
fib (I# n) = I# (fib' n)

-- Unboxed ints:
fib' :: Int# -> Int#
fib' 0# = 0#
fib' 1# = 1#
fib' n = fib' (n -# 2#) +# fib' (n -# 1#)

fibonaccis_seq :: [Int] -> [Int]
fibonaccis_seq = map fib

fibonaccis_par :: [Int] -> [Int]
fibonaccis_par xs = map fib xs `using` parList rdeepseq

fibonaccis_parChunk :: Int -> [Int] -> [Int]
fibonaccis_parChunk chunkSize xs = map fib xs `using` s
  where s = parListChunk chunkSize rdeepseq
