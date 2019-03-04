{-# LANGUAGE MagicHash #-}

module Fib
    ( fibonaccis_seq
    , fibonaccis_par
    ) where

import GHC.Exts -- Int# etc
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)

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

fibonaccis_par :: Int -> [Int] -> [Int]
fibonaccis_par chunkSize xs =
  fibonaccis_seq xs `using` parListChunk chunkSize rdeepseq
