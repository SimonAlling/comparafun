module Data.Vector.Split (chunkInto) where

import Prelude hiding (take, drop, null)
import Data.Vector (Vector(..), cons, take, drop, empty, null)

chunkInto :: Int -> Vector a -> Vector (Vector a)
chunkInto n vector = chunksOf (length vector `div` n) vector

chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf n vector
  | null vector = empty
  | otherwise   = take n vector `cons` chunksOf n (drop n vector)
