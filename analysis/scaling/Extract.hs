{-# LANGUAGE LambdaCase #-}

import Text.RegexPR
import Safe
import Debug.Trace

type TimeInMilliseconds = Int -- ms

data Time = Time Double Unit
  deriving Show

data Unit = Milliseconds | Seconds
  deriving Show

errorMessage :: String
errorMessage = "Could not extract time in milliseconds"

timeRegex :: String
timeRegex = "time\\s+(\\d+(?:\\.\\d+)?) ([m]?s)"

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

extractTime :: String -> Maybe Time
extractTime s = do
    groupMatches <- snd <$> matchRegexPR timeRegex s
    unit <- readMaybeUnit =<< lookup group_unit groupMatches
    number <- readMaybeNumber =<< lookup group_number groupMatches
    pure (Time number unit)

processRaw :: String -> Maybe TimeInMilliseconds
processRaw = fmap normalize . extractTime

transform :: String -> String
transform = ('\n':) . maybe errorMessage show . processRaw

main :: IO ()
main = interact transform