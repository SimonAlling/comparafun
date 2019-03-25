{-# LANGUAGE FlexibleInstances #-}

module KMeansStuff where

import Data.Metric (Metric(..))
import Data.Vector (Vector(..))
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose)
import Algorithms.Lloyd.Sequential (Point(..))

newtype Dimensions = D Int
  deriving (Eq, Show)

instance Arbitrary Dimensions where
  arbitrary = D <$> choose (1, 5)

instance Metric (Vector Double) where
  distance = squareDistance

squareDistance :: Num a => Vector a -> Vector a -> a
squareDistance p q = V.sum $ V.zipWith (\x y -> (x-y)^2) p q

genVector :: Arbitrary a => Dimensions -> Gen (Vector a)
genVector (D d) = V.replicateM d arbitrary

genPoint :: Dimensions -> Gen Point
genPoint = fmap Point . genVector
