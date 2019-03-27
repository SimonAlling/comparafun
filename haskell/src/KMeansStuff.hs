{-# LANGUAGE FlexibleInstances #-}

module KMeansStuff where

import Data.Metric (Metric(..))
import Data.Vector (Vector(..))
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose)
import Algorithms.Lloyd.Sequential (Point(..))
import Data.Function (on)
import Util ((<$$>))

newtype Dimensions = D Int
  deriving (Eq, Show)

unDim :: Dimensions -> Int
unDim (D d) = d

instance Enum Dimensions where
  fromEnum (D d) = d
  toEnum = D

instance Ord Dimensions where
  compare = compare `on` unDim

instance Num Dimensions where
  (+) = D <$$> ((+) `on` unDim)
  (*) = D <$$> ((*) `on` unDim)
  abs = D . abs . unDim
  signum = D . signum . unDim
  fromInteger = D . fromIntegral
  negate = D . negate . unDim

instance Real Dimensions where
  toRational = toRational . unDim

instance Integral Dimensions where
  quotRem (D a) (D b) = let (x, y) = quotRem a b in (D x, D y)
  toInteger = toInteger . unDim

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
