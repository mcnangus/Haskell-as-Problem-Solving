--
-- COMP1100/1130, Semester 1, 2018
--

module ColourName where

import           CodeWorld

data ColourName
  = Magenta
  | Black
  | Green
  | Yellow
  | Orange
  | Cyan
  deriving (Show)

--
-- | Black Colour Test
--
-- >>> colourNameToColour Black
-- RGBA 0.0 0.0 0.0 1.0

--
-- | Green Colour Test
--
-- >>> colourNameToColour Green
-- RGBA 0.125 0.875 0.125 1.0

--
-- | Orange Colour Test
--
-- >>> colourNameToColour Orange
-- RGBA 0.875 0.5 0.125 1.0

colourNameToColour :: ColourName -> Colour
colourNameToColour x = case x of
    Cyan    -> cyan -- Takes the data type ColourName and converts to the CodeWorld Colour type
    Orange  -> orange
    Yellow  -> yellow
    Green   -> green
    Black   -> black
    Magenta -> magenta

colourKeyMap :: [(String, ColourName)]
colourKeyMap =
    [("M", Magenta),
     ("B", Black),
     ("G", Green),
     ("Y", Yellow),
     ("O", Orange),
     ("C", Cyan)]
