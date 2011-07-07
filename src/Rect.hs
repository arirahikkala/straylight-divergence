module Rect where

import BasicTypes
-- basic business having to do with rectangles, will probably have a bunch of stuff transferred in from Mapgen.Rect with cleanups at some point
rectCoords (Coord xStart yStart, Coord xEnd yEnd) =
    [Coord x y | x <- [xStart .. xEnd - 1], y <- [yStart .. yEnd - 1]]
