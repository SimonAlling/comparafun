module Util
    ( printHECs
    ) where

import GHC.Conc (getNumCapabilities)

printHECs :: IO ()
printHECs = do
  n <- getNumCapabilities
  putStrLn $ "Using "++show n++" HECs"
