--
-- COMP1100/1130, Semester 1, 2018
--

module Main where
import CodeWorld

main :: IO ()
main = drawingOf ((translated 3 3 (solidCircle 2)) & (translated 4 4 (coloured red (solidRectangle (-3) 6))))
