module Data.Metric (Metric(..)) where

-- Metric class and instance defined by me, because I can't get the metric package to install.
class Metric a where
  distance :: a -> a -> Double

-- instance Metric (Vector Double) where
--   distance
