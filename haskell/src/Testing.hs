{-# LANGUAGE RebindableSyntax #-}

module Testing where

import Prelude
import Test.QuickCheck (Arbitrary, Property, Gen, choose, listOf, vectorOf, arbitrary, generate, forAll)
import KMeans

file_testDataForKMeans :: FilePath
file_testDataForKMeans = "app/KMeans/KMeansTestData.hs"

file_testDataForSorting :: FilePath
file_testDataForSorting = "app/Sort/SortTestData.hs"

instance Arbitrary Dimensions where
  arbitrary = D <$> choose (1, 5)

generateTestDataForKMeans :: Dimensions -> Int ->  IO ()
generateTestDataForKMeans dim n = do
  generate (genData dim n) >>= writeFile file_testDataForKMeans . wrap "KMeans"
  printNotice file_testDataForKMeans

generateTestDataForSorting :: Int ->  IO ()
generateTestDataForSorting n = do
  generate (vectorOf n (arbitrary :: Gen Int)) >>= writeFile file_testDataForSorting . wrap "Sort"
  printNotice file_testDataForSorting

wrap :: Show testData => String -> testData -> String
wrap ident testData = unlines $ do
  "module "++ident++"TestData where"
  ""
  "testData = " ++ show testData
  [] where (>>) = (:)

printNotice :: FilePath -> IO ()
printNotice file = putStrLn $ "Test data written to `" ++ file ++ "`."

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
