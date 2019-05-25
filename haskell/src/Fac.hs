module Fac
    ( factorials_seq
    , factorials_par
    ) where

import Control.Parallel.Strategies (rdeepseq, using, parList)

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

factorials_seq :: [Integer] -> [Integer]
factorials_seq = map fac

factorials_par :: [Integer] -> [Integer]
factorials_par xs = map fac xs `using` parList rdeepseq
