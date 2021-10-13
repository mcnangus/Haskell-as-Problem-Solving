--
-- COMP1100/1130, Semester 1, 2018
--

{-# LANGUAGE OverloadedStrings #-}

module Events where

import CodeWorld hiding (trace)
import Debug.Trace
import Shape
import ColourName
import State
import Data.Maybe
import Data.Text
import View

--
-- | initialState Test
--
-- >>> initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Rectangle Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> handleEvent (KeyPress "R") initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Incorrect Shape Input Test
--
-- >>> :set -XOverloadedStrings
-- >>> handleEvent (KeyPress "X") initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Magenta Colour Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "M") state
-- World [] (RectangleTool Nothing) Magenta

--
-- | Green Colour Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "G") state
-- World [] (RectangleTool Nothing) Green

--
-- | Incorrect Colour Input Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "H") state
-- World [] (RectangleTool Nothing) Black

--
-- | MousePress Event Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Cyan
-- >>> handleEvent (MousePress LeftButton (-3, 3)) state
-- World [] (RectangleTool (Just (-3.0,3.0))) Cyan

--
-- | MouseRelease Event Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool (Just (-3.0,3.0))) Cyan
-- >>> handleEvent (MouseRelease LeftButton (3, -3)) state
-- World [Graphic (Rectangle 6.0 6.0) Cyan (0.0,0.0)] (RectangleTool Nothing) Cyan

--
-- | Polygon Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> handleEvent (KeyPress "P") initialState
-- World [] (PolygonTool []) Black

--
-- | Polygon Colour Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> :set -XOverloadedStrings
-- >>> let state = World [] (PolygonTool []) Black
-- >>> handleEvent (KeyPress "O") state
-- World [] (PolygonTool []) Orange

--
-- | Polygon Vertex 1 Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool []) Orange
-- >>> handleEvent (MousePress LeftButton (3, 5)) state
-- World [] (PolygonTool [(3.0,5.0)]) Orange

--
-- | Polygon Vertex 2 Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool [(3.0,5.0)]) Green
-- >>> handleEvent (MousePress LeftButton (6, 8)) state
-- World [] (PolygonTool [(6.0,8.0),(3.0,5.0)]) Green

--
-- | Polygon Vertex 3 Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool [(6.0,8.0),(3.0,5.0)]) Green
-- >>> handleEvent (MousePress LeftButton (9, 2)) state
-- World [] (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Green

--
-- | Polygon Draw Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow
-- >>> handleEvent (KeyPress " ") state
-- World [Graphic (Polygon [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow (0.0,0.0)] (PolygonTool []) Yellow

--
-- | Shape Removal Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> import Graphic
-- >>> let state = World [(Graphic (Rectangle 6 6) Cyan (0, 0)), (Graphic (Rectangle 3 3) Magenta (8, 4))] (RectangleTool Nothing) Orange
-- >>> handleEvent (KeyPress "Backspace") state
-- World [Graphic (Rectangle 3.0 3.0) Magenta (8.0,4.0)] (RectangleTool Nothing) Orange

handleEvent :: Event -> State -> State
handleEvent e s =
  case (e, s) of
    (KeyPress key, (World a b c))
      | key == "Esc" -> initialState
      | key == "D"   -> trace (show s) s

      -- Checks if the key pressed is a tool key,
      -- then returns the new state with that tool
      | key == "R" || key == "E" || key == "P" || key == "L"
        -> World a (stringMap (unpack key) toolKeyMap initialTool) c

      -- Checks if the key pressed is a colour name key,
      -- then returns the new state with that colour name
      | key == "M" || key == "B" || key == "G" || key == "Y" || key == "O" || key == "C"
        -> World a b (stringMap (unpack key) colourKeyMap initialColour)

      -- Checks if the key pressed is backspace, then
      -- returns the new state without the graphic
      -- most recently drawn
      | key == "Backspace" && (show a) /= "[]"
        -> World (Prelude.tail a) b c

      where
        -- Takes a string input and a structured list
        -- of tuples and a default value and returns
        -- the value stored in the second element of
        -- the tuple with the same string as the input
        stringMap :: String -> [(String, a)] -> a -> a
        stringMap x y z = fromMaybe z (lookup x y)

    -- Checks if the selected tool is not PolygonTool
    -- after left clicking, returns the new state and its
    -- altered tool with the starting point coordinates
    ((MousePress LeftButton newPoint), (World a b c))
      | show b == show (RectangleTool Nothing)
        -> World a (RectangleTool (Just newPoint)) c
      | show b == show (EllipseTool Nothing)
        -> World a (EllipseTool (Just newPoint)) c
      | show b == show (LineTool Nothing)
        -> World a (LineTool (Just newPoint)) c

    -- Checks if the selected tool is the PolygonTool
    -- after left clicking, returns the new state and
    -- its new vertex for the current polygon
    ((MousePress LeftButton newPoint), (World a (PolygonTool b) c))
      -> World a (PolygonTool ([newPoint] ++ b)) c

    -- Checks if the selected tool is not PolygonTool
    -- after releasing the left click button, calls
    -- drawNewGraphic with the current state and the
    -- endpoint, returns the new state with graphics
    -- updated and tool reset
    ((MouseRelease LeftButton d), (World a (RectangleTool b) c))
      -> drawNewGraphic (World a (RectangleTool b) c) (Just d)
    ((MouseRelease LeftButton d), (World a (EllipseTool b) c))
      -> drawNewGraphic (World a (EllipseTool b) c) (Just d)
    ((MouseRelease LeftButton d), (World a (LineTool b) c))
      -> drawNewGraphic (World a (LineTool b) c) (Just d)

    -- Checks if the spacebar has been pressed
    -- whilst the polygonTool is selected, calls
    -- drawNewGraphic with the current state and
    -- Nothing, returns the new state with graphics
    -- updated and tool reset
    (KeyPress " ", (World a (PolygonTool b) c))
      -> drawNewGraphic (World a (PolygonTool b) c) Nothing
    _ -> s
