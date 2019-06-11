{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}

module Svg where

import Prelude hiding ((>>))
import Data.Foldable (toList)
import Data.List (intercalate)

(>>) = (:)

data Attributes = Attrs [(String, String)]

instance Show Attributes where
  show (Attrs as) = concat $ map (\(name, value) -> concat [ " ", name, "=\"", value, "\"" ]) as

noAttrs :: Attributes
noAttrs = Attrs []

data Node
  = TextNode String
  | EmptyElement String Attributes
  | Element String Attributes [Node]

instance Show Node where
  show (TextNode s) = s
  show (EmptyElement tagName attrs) = concat [ "<", tagName, show attrs, "/>" ]
  show (Element tagName attrs children) = concat [ "<", tagName, show attrs, ">", "\n", content, "</", tagName, ">" ]
    where content = indent 4 $ unlines $ map show children

data Color = Red | ForestGreen | Blue | Orange | Magenta | Brown | Purple | DodgerBlue
  deriving (Enum, Show)

colors :: [Color]
colors = enumFrom Red

class_points :: String
class_points = "points"

data RenderConfig = RenderConfig
  { chartSize :: Int
  , pointRadius :: Double
  , margin :: Int
  , scalingFactor :: Double
  }

defaultRenderConfig :: RenderConfig
defaultRenderConfig = RenderConfig
  { chartSize = 100
  , pointRadius = 1
  , margin = 10
  , scalingFactor = 1
  }

plotIO :: (Num n, Show n, Foldable t, Foldable u) => String -> RenderConfig -> (a -> (n, n)) -> t (u a) -> IO ()
plotIO filename config f = writeFile filename . plot config f

plot :: (Num n, Show n, Foldable t, Foldable u) => RenderConfig -> (a -> (n, n)) -> t (u a) -> String
plot config f = show . svg config . pure . chart config . plotClusters config f

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

svg :: RenderConfig -> [Node] -> Node
svg config = Element "svg" $ Attrs $ do
  ("xmlns", "http://www.w3.org/2000/svg")
  ("xmlns:xlink", "http://www.w3.org/1999/xlink")
  ("version", "1.1")
  ("width", size)
  ("height", size)
  []
  where size = show $ scalingFactor config * fromIntegral (chartSize config + 2 * margin config)

chart :: RenderConfig -> [Node] -> Node
chart config children = Element "g" attrs allChildren
  where
    attrs = Attrs $ do
      ("transform", concat [ "translate(",x,", ",y,") scale(",scaleX,", ",scaleY,")" ])
      []
    allChildren = (++ children) $ do
      EmptyElement "rect" $ Attrs $ do
        ("x", show 0)
        ("y", show 0)
        ("width", show 100)
        ("height", show 100)
        ("stroke", "#AAA")
        ("stroke-width", "1")
        ("fill", "none")
        []
      []
    scale = (*) (scalingFactor config)
    x = show . scale . fromIntegral $ margin config
    y = show . scale . fromIntegral $ margin config + chartSize config
    scaleX = show $ scale 1
    scaleY = show $ scale (-1)

point :: (Num n, Show n) => Color -> Double -> (n, n) -> Node
point c r (x, y) = EmptyElement "circle" $ Attrs $ do
  ("fill", show c)
  ("cx", show x)
  ("cy", show y)
  ("r", show r)
  []

plotClusters :: (Num n, Show n, Foldable t, Foldable u) => RenderConfig -> (a -> (n, n)) -> t (u a) -> [Node]
plotClusters config getCoords clusters = map plotClusterWithColor clustersAndColors
  where
    clustersAndColors = zip (toList clusters) $ cycle colors
    plotClusterWithColor (cluster, color) = Element "g" noAttrs (plotPoints config color getCoords cluster)

plotPoints :: (Num n, Show n, Foldable t) => RenderConfig -> Color -> (a -> (n, n)) -> t a -> [Node]
plotPoints config color f = map (point color r . f) . toList
  where r = pointRadius config

example :: [[(Int, Int)]]
example = [[(4, 6), (5, 1)], [(70, 50), (72, 59)]]