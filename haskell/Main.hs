module Main where

import Control.Parallel
import Criterion.Main (defaultMain, bench, nf)
import Control.Parallel.Strategies (parListChunk, rdeepseq, rseq, using)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Safe
import Control.Monad (join)

chunkSize :: Integral a => a
chunkSize = 100

chunks :: Integral a => a
chunks = 8

factorials_seq :: [Integer] -> [Integer]
factorials_seq = map fac

factorials_par :: [Integer] -> [Integer]
factorials_par xs = factorials_seq xs `using` parListChunk chunkSize rdeepseq

fac :: Integer -> Integer
fac n | n <= 0 = 1
fac n = n * fac (n - 1)

mainWith :: Integer -> IO ()
mainWith bigNumber =
  let
    numberOfItems = chunkSize * chunks
    integers = replicate numberOfItems bigNumber
  in
    defaultMain
      [ bench "factorials_seq" (nf factorials_seq integers)
      , bench "factorials_par" (nf factorials_par integers)
      ]

main :: IO ()
main =
  let
    readMaybeInt = readMaybe :: String -> Maybe Integer
    printHelp = putStrLn "Usage: ./Main 4000 factorials"
  in do
    maybeBigNumber <- (readMaybeInt =<<) . headMay <$> getArgs
    maybe printHelp mainWith maybeBigNumber
