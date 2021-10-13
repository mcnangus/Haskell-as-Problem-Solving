--
-- COMP1100/1130, Semester 1, 2018
--
module View where

import CodeWorld
import ColourName
import Graphic
import Shape
import State

import qualified Data.Text as Text

drawState :: State -> Picture
drawState (World gs tool colour) =
  pictures $ shapeToPicture : colourToPicture : graphicsToPics gs
  where
    shapeToPicture, colourToPicture :: Picture
    shapeToPicture =
      translated (-13.5) 8 $ (text . Text.pack) ("Shape: " ++ shapeToText)
    colourToPicture =
      translated (-13.5) 7 $ (text . Text.pack) ("Colour: " ++ colourToText)
    shapeToText :: String
    shapeToText =
      let shape = takeWhile (/= ' ') $ show tool
       in take (length shape - 4) shape
    colourToText :: String
    colourToText = show colour

--
-- | drawNewGraphic Test 1
--
-- >>> drawNewGraphic (World [] (RectangleTool (Just (-3, 3))) Black) (Just (3, -3))
-- World [Graphic (Rectangle 6.0 6.0) Black (0.0,0.0)] (RectangleTool Nothing) Black
-- 
-- | drawNewGraphic Test 2
-- 
-- >>> drawNewGraphic (World [] (RectangleTool (Just (0, 0))) Orange) (Just (3, 7))
-- World [Graphic (Rectangle 3.0 7.0) Orange (1.5,3.5)] (RectangleTool Nothing) Orange
drawNewGraphic :: State -> Maybe Point -> State
drawNewGraphic s mp = case (s, mp) of
    -- Creates and adds the new graphic to the
    -- start of the graphics list in the new state
    ((World a (RectangleTool (Just b)) c), Just d)
      -> World ([(getRectangleGraphic b d c)] ++ a) (RectangleTool Nothing) c
    ((World a (EllipseTool (Just b)) c), Just d)
      -> World ([(getEllipseGraphic b d c)] ++ a) (EllipseTool Nothing) c
    ((World a (LineTool (Just b)) c), Just d)
      -> World ([(getLineGraphic b d c)] ++ a) (LineTool Nothing) c
    ((World a (PolygonTool b) c), Nothing)
      -> World ([(getPolygonGraphic b c)] ++ a) (PolygonTool []) c
    _ -> s

getNewGraphic :: State -> Maybe Point -> Maybe Graphic
getNewGraphic = undefined -- TODO

getRectangleGraphic :: Point -> Point -> ColourName -> Graphic -- Takes a start and end point for a Rectangle and calculates the length and width, sets a colour, and finds the midpoint between the start and end points
getRectangleGraphic (a, aa) (b, bb) c = Graphic (Rectangle (abs (b-a)) (abs (bb-aa))) c ((b+a)/2, (bb+aa)/2)

getEllipseGraphic :: Point -> Point -> ColourName -> Graphic -- Uses same logic as getRectangleGraphic but is used for an ellipse
getEllipseGraphic (a, aa) (b, bb) c = Graphic (Ellipse (abs (b-a)) (abs (bb-aa))) c ((b+a)/2, (bb+aa)/2)


getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic a b c = Graphic (Line a b) c (0, 0) -- Takes a start and end point for a Line, sets a colour

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic a b = Graphic (Polygon (a)) b (0, 0) -- Takes a list of points for vertices of a Polygon, sets a colour

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift = undefined -- TODO

shapeToPic :: Shape -> Picture
shapeToPic x = case x of
    (Line a b)      -> polyline [a, b] -- Converts a Shape Line to a Picture with a start and end point
    (Polygon a)     -> solidPolygon a -- Converts a Shape Polygon to a Picture with a list of vertices
    (Ellipse a b)   -> scaled a b (solidCircle 0.5) -- Converts a Shape Ellipse into a scaled Picture of a circle with length and width
    (Rectangle a b) -> solidRectangle a b -- Converts a Shape Rectangle into a Picture with width and length

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic

graphicToPic :: Graphic -> Picture
graphicToPic x = case x of
    Graphic a b (c, d) -> (translated  c d (coloured (colourNameToColour b) (shapeToPic a))) -- Coverts the Graphic data type into a CodeWorld Picture type
