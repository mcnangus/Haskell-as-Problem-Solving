--
-- COMP1100/1130, Semester 1, 2018
--

module Graphic where
    
import Shape
import ColourName
import CodeWorld (Point)

data Graphic =
  Graphic Shape
          ColourName
          Point
  deriving (Show)
