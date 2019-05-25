{-# LANGUAGE MagicHash #-}

module Fac
    ( factorials_seq
    , factorials_par
    ) where

import GHC.Exts -- Int# etc
import Control.Parallel.Strategies (rdeepseq, using, parList)

-- Boxed ("regular") ints:
fac :: Int -> Int
fac (I# n) = I# (fac' n)

-- Unboxed ints:
fac' :: Int# -> Int#
fac' 0# = 1#
fac' n = n *# fac' (n -# 1#)

factorials_seq :: [Int] -> [Int]
factorials_seq = map fac

factorials_par :: [Int] -> [Int]
factorials_par xs = map fac xs `using` parList rdeepseq
