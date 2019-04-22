module Data.Metric (Metric(..), Euclidean(..)) where

-- Copied by me (Simon Alling) from the metric package, because I couldn't get
-- it to build.

import Prelude hiding (zipWith, map, sum)
import Data.Function (on)
import Data.Vector (Vector(..), zipWith, map, sum)
import Control.Applicative.Extras ((<$$>))

class Metric a where
  distance :: a -> a -> Double

newtype Euclidean = Euclidean
  { getEuclidean :: Vector Double
  } deriving (Eq, Show)

instance Metric Euclidean where
  distance = sqrt . sum . map (**2) <$$> zipWith (-) `on` getEuclidean
