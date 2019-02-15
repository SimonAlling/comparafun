{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import Control.Parallel
import Criterion.Main (defaultMain, bench, nf)
import Control.Parallel.Strategies (parListChunk, rdeepseq, rseq, using)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Safe

chunkSize :: Integral a => a
chunkSize = 32

chunks :: Integral a => a
chunks = 8

factorials_seq :: [Integer] -> [Integer]
factorials_seq = map fac

factorials_par :: [Integer] -> [Integer]
factorials_par xs = factorials_seq xs `using` parListChunk chunkSize rdeepseq

fibonaccis_seq :: [Int] -> [Int]
fibonaccis_seq = map fib

fibonaccis_par :: [Int] -> [Int]
fibonaccis_par xs = fibonaccis_seq xs `using` parListChunk chunkSize rdeepseq

fac :: Integer -> Integer
fac n | n <= 0 = 1
fac n = n * fac (n - 1)

-- Boxed ints:
fib :: Int -> Int
fib (I# n) = I# (fib' n)

-- Unboxed ints:
fib' :: Int# -> Int#
fib' 0# = 0#
fib' 1# = 1#
fib' n = fib' (n -# 2#) +# fib' (n -# 1#)

mainWith :: Integer -> IO ()
mainWith bigNumber =
  let
    numberOfItems = chunkSize * chunks
    integers = replicate numberOfItems bigNumber
    ints = map fromInteger integers
  in
    defaultMain
      [ bench "factorials_seq" (nf factorials_seq integers)
      , bench "factorials_par" (nf factorials_par integers)
      , bench "fibonaccis_seq" (nf fibonaccis_seq ints)
      , bench "fibonaccis_par" (nf fibonaccis_par ints)
      ]

main :: IO ()
main =
  let
    readMaybeInt = readMaybe :: String -> Maybe Integer
    printHelp = putStrLn "Usage: ./Main 30 fibonaccis +RTS -H1G -A100M -N2"
  in do
    maybeBigNumber <- (readMaybeInt =<<) . headMay <$> getArgs
    maybe printHelp mainWith maybeBigNumber
