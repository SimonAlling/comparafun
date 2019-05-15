{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Testing where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Random.Source.PureMT
import Data.Random (Uniform, Distribution, uniform, sample)
import Control.Monad.State (evalState, replicateM)
import GHC.Word (Word64)
import Util (compose3)

type Seed = Word64
type Size = Int
type Interval a = (a, a)
type Dimensions = Int
type Threads = Int

class SuiteConfig c where
  problemSizes :: c -> [Int]

-- The a is intended to hold something like a number of chunks to split the work into.
data Parallelism a = Sequential | Parallel a

instance Show a => Show (Parallelism a) where
  show = \case
    Sequential -> "sequential"
    Parallel p -> show p

listFromSeed :: (Integral int, Num a, Distribution Uniform a) => Word64 -> (a, a) -> int -> [a]
listFromSeed seed (lo, hi) n = evalState st $ pureMT seed
  where st = replicateM (fromIntegral n) $ sample $ uniform lo hi

vectorFromSeed :: (Num a, Distribution Uniform a) => Seed -> (a, a) -> Dimensions -> V.Vector a
vectorFromSeed = compose3 V.fromList listFromSeed
