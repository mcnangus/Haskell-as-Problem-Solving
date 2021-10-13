--
-- COMP1100/1130, Semester 1, 2018
--

module Shape where

import CodeWorld (Point)

data Shape = Rectangle Side Side
           | Ellipse   Side Side
           | Polygon   [Point]
           | Line      Point Point
  deriving (Show)

data Tool = RectangleTool (Maybe Point)
          | EllipseTool   (Maybe Point)
          | LineTool      (Maybe Point)
          | PolygonTool   [Point]
  deriving (Show)

toolKeyMap :: [(String, Tool)]
toolKeyMap = [
  ("R", RectangleTool Nothing),
  ("E", EllipseTool Nothing),
  ("P", PolygonTool []),
  ("L", LineTool Nothing)
  ]

type Side = Double