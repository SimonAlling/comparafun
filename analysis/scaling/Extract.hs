{-# LANGUAGE LambdaCase #-}

import Text.RegexPR
import Safe
import Debug.Trace
import System.Environment
import Data.List (intercalate)

type TimeInMilliseconds = Int -- ms
type Line = String
type Regex = String

data Time = Time Double Unit
  deriving Show

data Unit = Milliseconds | Seconds
  deriving Show

timeRegexHaskell :: Regex
timeRegexHaskell = "time\\s+(\\d+(?:\\.\\d+)?) ([m]?s)"

timeRegexScala :: Regex
timeRegexScala = "mean = (\\d+(?:\\.\\d+)?) ([m]?s)"

group_number, group_unit :: Int
group_number = 1
group_unit   = 2

readMaybeUnit :: String -> Maybe Unit
readMaybeUnit = \case
  "ms" -> pure Milliseconds
  "s"  -> pure Seconds
  _    -> Nothing

readMaybeNumber :: String -> Maybe Double
readMaybeNumber = readMay

normalize :: Time -> TimeInMilliseconds
normalize (Time number unit) = round $ case unit of
  Milliseconds -> number
  Seconds      -> number * 1000

extractTime :: Regex -> String -> Maybe Time
extractTime regex s = do
    groupMatches <- snd <$> matchRegexPR regex s
    unit <- readMaybeUnit =<< lookup group_unit groupMatches
    number <- readMaybeNumber =<< lookup group_number groupMatches
    pure (Time number unit)

processLine :: Regex -> Line -> Maybe TimeInMilliseconds
processLine regex = fmap normalize . extractTime regex

transform :: Regex -> String -> String
transform regex = unlines . filter ((>0) . length) . map (maybe "" show . processLine regex) . lines

languages :: [(String, Regex)]
languages = [("haskell", timeRegexHaskell), ("scala", timeRegexScala)]

main :: IO ()
main = do
  firstArg <- headMay <$> getArgs
  let regex = firstArg >>= flip lookup languages
  let specifyArgument = putStrLn $ "Please specify one of these: " ++ intercalate ", " (map fst languages)
  maybe specifyArgument (interact . transform) regex