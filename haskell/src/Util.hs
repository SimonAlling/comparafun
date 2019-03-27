module Util
    ( printHECs
    , compose2
    , compose3
    , (<$$>)
    , (<$$$>)
    ) where

import GHC.Conc (getNumCapabilities)

compose2 :: (y -> z) -> (a -> b -> y) -> (a -> b -> z)
compose2 = (.).(.)

(<$$>) = compose2

compose3 :: (y -> z) -> (a -> b -> c -> y) -> (a -> b -> c -> z)
compose3 = (.).(.).(.)

(<$$$>) = compose3

printHECs :: IO ()
printHECs = do
  n <- getNumCapabilities
  putStrLn $ "Using "++show n++" HECs"
