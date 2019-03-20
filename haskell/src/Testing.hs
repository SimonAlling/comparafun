{-# LANGUAGE RebindableSyntax #-}

module Testing where

import Prelude
import Test.QuickCheck (Arbitrary, Property, Gen, choose, listOf, vectorOf, arbitrary, generate, forAll)
import KMeans

file_testData :: FilePath
file_testData = "app/KMeans/TestData.hs"

instance Arbitrary Dimensions where
  arbitrary = D <$> choose (1, 5)

generateTestData :: Dimensions -> Int ->  IO ()
generateTestData dim n = do
  generate (genData dim n) >>= writeFile file_testData . wrap
  putStrLn $ "Test data written to `" ++ file_testData ++ "`."

wrap :: Show testData => testData -> String
wrap testData = unlines $ do
  "module TestData where"
  ""
  "testData = " ++ show testData
  [] where (>>) = (:)

genPoint :: Dimensions -> Gen Point
genPoint (D d) = vectorOf d arbitrary

genPoints :: Dimensions -> Gen [Point]
genPoints = listOf . genPoint

genData :: Dimensions -> Int -> Gen [Point]
genData dim numOfPoints = vectorOf numOfPoints (genPoint dim)

-- QuickCheck property
prop_correct :: Dimensions -> Int -> Property
prop_correct dimensions k = forAll (genPoints dimensions) (flip correctFor k)

correctFor :: [Point] -> Int -> Bool
correctFor points k = k <= 0 || kmeans_seq k points == theirkmeans k points
