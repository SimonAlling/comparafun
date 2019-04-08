module Util
    ( printHECs
    , printCPUInfo
    , printSystemInfo
    , compose2
    , compose3
    , (<$$>)
    , (<$$$>)
    , equality
    ) where

import GHC.Conc (getNumCapabilities, getNumProcessors)

compose2 :: (y -> z) -> (a -> b -> y) -> (a -> b -> z)
compose2 = (.).(.)

(<$$>) = compose2

compose3 :: (y -> z) -> (a -> b -> c -> y) -> (a -> b -> c -> z)
compose3 = (.).(.).(.)

(<$$$>) = compose3

printSystemInfo :: IO ()
printSystemInfo = printCPUInfo >> printHECs

printHECs :: IO ()
printHECs = do
  n <- getNumCapabilities
  putStrLn $ "Using "++show n++" HEC"++(if n > 1 then "s" else "")

printCPUInfo :: IO ()
printCPUInfo = do
  t <- getNumProcessors
  putStrLn $ "Detected " ++ show t ++ " hardware thread" ++ (if t > 1 then "s" else "")

equality :: Show a => String -> a -> String
equality name value = name ++ " = " ++ show value
