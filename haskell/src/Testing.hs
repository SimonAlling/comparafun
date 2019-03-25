{-# LANGUAGE RebindableSyntax #-}

module Testing where

import Prelude
import Test.QuickCheck (Arbitrary, Property, Gen, choose, listOf, vectorOf, arbitrary, generate, forAll)
import Data.Metric (Metric, distance)
import Data.Vector hiding ((++), generate)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), ExpectDivergent(..), kmeans)
import qualified Algorithms.Lloyd.Strategies as LloydPar
import KMeansStuff (Dimensions(..), squareDistance, genPoint, genVector)

file_testDataForKMeans :: FilePath
file_testDataForKMeans = "app/KMeans/KMeansTestData.hs"

file_testDataForSorting :: FilePath
file_testDataForSorting = "app/Sort/SortTestData.hs"

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

genPoints :: Dimensions -> Gen [Point]
genPoints = listOf . genPoint

genData :: Dimensions -> Int -> Gen [Point]
genData dim numOfPoints = vectorOf numOfPoints (genPoint dim)

-- QuickCheck property
prop_correct :: Dimensions -> Int -> Property
prop_correct dimensions k = forAll (genPoints dimensions) (flip correctFor k)

correctFor :: [Point] -> Int -> Bool
correctFor points k = k <= 0 || True

kmeansExampleResult :: Vector (Vector Point)
kmeansExampleResult = kmeans expectDivergent metric points initial
  where
    expectDivergent :: ExpectDivergent
    expectDivergent = ExpectDivergent 10
    metric :: Vector Double -> Vector Double
    metric = id
    points :: Vector Point
    points = undefined
    initial :: Vector Cluster
    initial = undefined

initialize :: Int -> Vector Point -> Vector Cluster
initialize k points = V.map (\x -> Cluster (snd x) undefined) $ pointsAndIds
  where
    pointsAndIds = V.zip points (V.fromList $ cycle [1..k])
