module Main where

import Criterion.Main (defaultMain, bench, nf)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Safe

import Fib

chunkSize :: Integral a => a
chunkSize = 32

chunks :: Integral a => a
chunks = 32

mainWith :: Integer -> IO ()
mainWith bigNumber =
  let
    numberOfItems = chunkSize * chunks
    integers = replicate numberOfItems bigNumber
    ints = map fromInteger integers
  in
    defaultMain
      [ bench "fibonaccis_seq" (nf fibonaccis_seq ints)
      , bench "fibonaccis_par" (nf (fibonaccis_par chunkSize) ints)
      ]

main :: IO ()
main =
  let
    readMaybeInt = readMaybe :: String -> Maybe Integer
    printHelp = putStrLn "Usage: ./Main 30 fibonaccis +RTS -H1G -A100M -N2"
  in do
    maybeBigNumber <- (readMaybeInt =<<) . headMay <$> getArgs
    maybe printHelp mainWith maybeBigNumber
