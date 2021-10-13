--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld hiding (Point)
import Data.Text (pack)
import Model

-- a pixel coordinate is a pair of Int
type Coord = (Int, Int)

-- a pixel value is some shade of grey (0.0 == white, 1.0 = black)
type Shade = Double

-- a raster pixel is represented by a coordinate and a shade
type Pixel = (Coord, Shade)

-- a raster is a list of pixels
type Raster = [Pixel]

-- $setup
-- >>> import Data.List (sort)

coordToPoint :: Resolution -> Coord -> Point
coordToPoint z (x, y) = (x', y')
  where
    x' = fromIntegral x * z
    y' = fromIntegral y * z

pointToCoord :: Resolution -> Point -> Coord
pointToCoord z (x, y) = (x', y')
  where
    x' = round $ x / z
    y' = round $ y / z

-- Update the view based on the model by constructing a rasterised CodeWorld picture
updateView :: Model -> Picture
updateView (Model ss z s t) =
  coordinatePlane &
  pictures (map pixelToPicture $ concatMap (shapeToRaster z s) ss) &
  translated (-13.5) 8 $
  (text . pack) ("Shape: " ++ shapeToText)
  where
    shapeToText = take (length shape - 4) shape
    shape = takeWhile (/= ' ') $ maybe "" show t
    pixelToPicture (c, b) = translated x' y' p
      where
        p = coloured (grey $ 1 - b) $ solidRectangle z z
        (x', y') = coordToPoint z c

-- Construct a raster for a shape at the given resolution (optionally smoothed)
shapeToRaster :: Resolution -> Smooth -> Shape -> Raster
shapeToRaster z s shape =
  case shape of
    Point p1 -> pointRaster $ pointToCoord z p1
    -- Pattern matches each shape and returns
    -- a rasterisation of that shape based
    -- on the points entered by the user
    Rectangle p1 p2 -> rectangleRaster (pointToCoord z p1) (pointToCoord z p2)
    Line p1 p2 -> lineRaster s (pointToCoord z p1) (pointToCoord z p2)

    -- For polygons, the first point in the list
    -- is added to the end before rasterisation
    Polygon pList -> polyLineRaster s [ pointToCoord z x | x <- pList ++ [head pList] ]
    Circle p1 p2 -> circleRaster s (pointToCoord z p1) (pointToCoord z p2)

-- | A raster for the point p
-- Examples:
-- >>> pointRaster (1,1)
-- [((1,1),1.0)]
pointRaster :: Coord -> Raster
pointRaster p = [(p, 1)]

-- | A raster for the rectangle with corner coordinates (x1,y1) and (x2,y2)
-- Examples:
-- >>> sort $ rectangleRaster (-1,-1) (1,1)
-- [((-1,-1),1.0),((-1,0),1.0),((-1,1),1.0),((0,-1),1.0),((0,1),1.0),((1,-1),1.0),((1,0),1.0),((1,1),1.0)]

-- Takes two corner coordinates and returns a rasterised image
-- of a rectangle's border with the two defined corners
rectangleRaster :: Coord -> Coord -> Raster
rectangleRaster (x1, y1) (x2, y2) = [ ((i, j),1.0) | i <- listBetween x1 x2,
                                                     j <- listBetween y1 y2,
                                                     isPerimeter (i,j) ]
    where
        -- Takes the coordinates for the corners of the
        -- rectangle and a possible new coordinate and
        -- returns True if the new coordinate is on the
        -- perimeter of the rectangle
        isPerimeter :: Coord -> Bool
        isPerimeter (x,y)
            | x1 == x || y1 == y || x2 == x || y2 == y = True
            | otherwise = False

-- | A raster for the line with end coordinates given as arguments.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ lineRaster False (-1,-1) (1,1)
-- [((-1,-1),1.0),((0,0),1.0),((1,1),1.0)]
--
-- prop> a == (fst $ head $ lineRaster False a b)
-- prop> b == (fst $ last $ lineRaster False a b)

-- Uses Bresenham's line algorithm to draw a
-- rasterisation of a line using two endpoints
lineRaster :: Smooth -> Coord -> Coord -> Raster
lineRaster _ (x1, y1) (x2, y2)
    -- Checks to see if the line runs more than it rises
    -- and then creates a line with a small gradient magnitude
    | abs (y2-y1) < abs (x2-x1) = [ ((i, j),1.0) | i <- listBetween x1 x2,
                                                   let j' = nearestValue (m * fromIntegral (i-x1) + fromIntegral y1),
                                                   j <- [j'] ]
    -- Creates a line with a large gradient magnitude
    | otherwise = [ ((i, j),1.0) | j <- listBetween y1 y2,
                                   let i' = nearestValue ((1/m) * fromIntegral (j-y1) + fromIntegral x1),
                                   i <- [i'] ]

    where
        -- Definition of the gradient of the
        -- line between the two endpoints
        m = fromIntegral (y2-y1) / fromIntegral (x2-x1)

-- | A raster for the polyline with vertices vs.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ polyLineRaster False [(0,0),(2,2)]
-- [((0,0),1.0),((1,1),1.0),((2,2),1.0)]
-- >>> sort $ polyLineRaster False [(0,0),(1,1),(0,1)]
-- [((0,0),1.0),((0,1),1.0),((1,1),1.0)]
--
-- prop> lineRaster False a b == polyLineRaster False [a, b]

-- Takes a list of coordinates and returns a
-- rasterisation of a polygon using lineRaster
polyLineRaster :: Smooth -> [Coord] -> Raster
polyLineRaster _ (a:b:c) = lineRaster False a b ++ polyLineRaster False (b:c)
polyLineRaster _ _ = []

--polyLineRaster _ coords = case coords of
--    x:y:z -> lineRaster False x y ++ polyLineRaster False (y:z)
--    [x] -> lineRaster False x
--    _ -> []

-- | A raster for the circle with center (x1,y1) and intersecting (x2,y2)
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ circleRaster False (0,0) (0,1)
-- [((-1,0),1.0),((0,-1),1.0),((0,1),1.0),((1,0),1.0)]

-- Takes a midpoint another point that lies on the
-- circle and returns a rasterisation of a circle
circleRaster :: Smooth -> Coord -> Coord -> Raster
circleRaster _ (h, k) (x1, y1) = fstQdt ++ map flipV fstQdt ++ map flipH (fstQdt ++ map flipV fstQdt)
    where
        -- Defines the radius from the midpoint and another point
        rad = sqrt (fromIntegral (x1-h)**2 + fromIntegral (y1-k)**2)
        -- Defines the value 45 degrees from the
        -- x-axis assuming the midpoint is at (0,0)
        oct = nearestValue (1/sqrt 2*rad)
        -- Generates a list of coordinates that
        -- lie on the first octant of the graph
        fstOct = [ ((x, y),1.0) | y <- [k..oct+k],
                                  let x' = nearestValue (sqrt (rad**2 - fromIntegral (y-k)**2) + fromIntegral h),
                                  x <- [x'] ]
        -- Generates a list of coordinates that
        -- lie on the second octant of the graph
        sndOct = [ ((x, y),1.0) | x <- [h..oct+h],
                                  let y' = nearestValue (sqrt (rad**2 - fromIntegral (x-h)**2) + fromIntegral k),
                                  y <- [y'] ]
        -- Combines the first two octants into a quadrant
        fstQdt = fstOct ++ sndOct
        -- Function that reflects a coordinate
        -- over the circle's x-axis
        flipV :: (Coord,Resolution) -> (Coord,Resolution)
        flipV ((x,y),z) = ((x,2*k-y),z)
        -- Function that reflects a coordinate
        -- over the circle's y-axis
        flipH :: (Coord,Resolution) -> (Coord,Resolution)
        flipH ((x,y),z) = ((2*h-x,y),z)

-- Takes two Ints and creates a list from the
-- smallest to the largest with an interval of one
listBetween :: Int -> Int -> [Int]
listBetween a b
    | a < b = [a..b]
    | otherwise = [b..a]

-- Rounds a Float to the nearest whole
-- number except x.5 goes to just x
nearestValue :: Float -> Int
nearestValue x
    | x - 0.5 <= fromInteger (truncate x) = floor x
    | otherwise = ceiling x


