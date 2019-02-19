import           Data.Maybe (fromMaybe)
import qualified Control.Monad.Parallel as ParM
import           Safe (headMay)
import           System.Process (readProcess)

version :: String
version = "110.81"

baseurl :: String
baseurl = "http://smlnj.cs.uchicago.edu/dist/working/" ++ version ++ "/"

main :: IO ()
main = ParM.mapM getHash urls >>= mapM_ putStrLn

getHash :: String -> IO String
getHash url = do
  stdout <- readProcess "nix-prefetch-url" [ url ] []
  pure $ fromMaybe ("empty stdout, URL: " ++ url) (headMay $ lines stdout)

urls :: [String]
urls = map (baseurl ++)
  [ "config.tgz"
  , "cm.tgz"
  , "compiler.tgz"
  , "runtime.tgz"
  , "system.tgz"
  , "MLRISC.tgz"
  , "smlnj-lib.tgz"
  , "old-basis.tgz"
  , "ckit.tgz"
  , "nlffi.tgz"
  , "cml.tgz"
  , "eXene.tgz"
  , "ml-lpt.tgz"
  , "ml-lex.tgz"
  , "ml-yacc.tgz"
  , "ml-burg.tgz"
  , "pgraph.tgz"
  , "trace-debug-profile.tgz"
  , "heap2asm.tgz"
  , "smlnj-c.tgz"
  , "doc.tgz"
  , "boot.x86-unix.tgz"
  ]
