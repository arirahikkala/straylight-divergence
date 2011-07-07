{-# LANGUAGE TemplateHaskell, NamedFieldPuns, ScopedTypeVariables #-}
module Mapgen.Rect where

import BasicTypes
import Data.List (intersect)
import Rect

{- 
   A BoundsRect, interpreted as a room, contains the inside of the room, and its bottom and right walls, full-inclusively.

   For instance: The room (Coord 0 0, Coord 0 0) contains a single wall tile.

   Marking walls as #, insides as ., and Coord 0 0 as *, this is the totality of the walls and insides of the room (Coord 0 0, Coord 1 1):

   ###
   #*#
   ###

   Similarly for (Coord 0 0, Coord 2 2):

   ####
   #*.#
   #..#
   ####

   These walls, altogether, are simply a room's *walls* (see the function (note to self: learn to link to functions with Haddock)).

   However, since the walls between rooms need to be only one tile thick, in fact rooms are usually only responsible for their lower and right walls, like so:

   *#    (Coord 0 0, Coord 2 2)
   ##

   *.#   (Coord 0 0, Coord 3 2)
   ### 

   *.#   (Coord 0 0, Coord 3 3)
   ..#
   ###

   These wall tiles, minus the corner one, are the room's connectiveTiles. Doors between two rooms are allowed in the tiles that are in one room's walls and another's connectiveTiles.

-}

approximateCenter (Coord bX bY, Coord eX eY) = Coord ((eX - bX) `div` 2 + bX) ((eY - bY) `div` 2 + bY)

connectionWidth r1 r2 = length $ connectiveWallTiles r1 r2

{- the walls around a room, including both those that it owns and those that it doesn't
   in order (for the room (Coord 0 0, Coord 1 1), with * = Coord 0 0)
   123
   8*4
   765

   The left and bottom walls are reversed, so the tiles go "around" the room clockwise. Each of the corners are included in the horizontal walls.
-}

walls :: BoundsRect -> [[Coord]]
walls (Coord xB yB, Coord xE yE) = 
    [[Coord x (yB - 1) | x <- [xB - 1 .. xE]],
     [Coord xE y | y <- [yB.. yE - 1]],
     reverse [Coord x yE | x <- [xB - 1 .. xE]],
     reverse [Coord (xB - 1) y | y <- [yB .. yE - 1]]]

wallTiles = concat . walls

wallsSansCorners :: BoundsRect -> [[Coord]]
wallsSansCorners (Coord xB yB, Coord xE yE) = 
    [[Coord x (yB - 1) | x <- [xB .. xE - 1]],
     [Coord xE y | y <- [yB.. yE - 1]],
     reverse [Coord x yE | x <- [xB .. xE - 1]],
     reverse [Coord (xB - 1) y | y <- [yB .. yE - 1]]]

wallTilesSansCorners = concat . wallsSansCorners

bottomRightCorner :: BoundsRect -> Coord
bottomRightCorner = snd

commonWallTiles room1 room2 =
    intersect (wallTiles room1) (wallTiles room2)

isCornerOf :: BoundsRect -> Coord -> Bool
isCornerOf (Coord bX bY, Coord eX eY) (Coord pX pY) =
    ((pX == bX) && (pY == bY)) ||
    ((pX == bX) && (pY == eY)) ||
    ((pX == eX) && (pY == bY)) ||
    ((pX == eX) && (pY == eY))

connectiveWallTiles room1 room2 = 
    (filter (\c -> (c `elem` wallTilesSansCorners room2))
     (connectiveTiles room1) ++
    filter (\c -> (c `elem` wallTilesSansCorners room1))
     (connectiveTiles room2))

rectContainsTile (Coord xPos yPos, (xSize, ySize)) (Coord xCoord yCoord) =
    xCoord >= xPos &&
    xCoord < xPos + xSize &&
    yCoord >= yPos &&
    yCoord < yPos + ySize

connectiveTiles (Coord xB yB, Coord xE yE) =
    concat $ 
    [[Coord x yE | x <- [xB .. xE - 1]], 
     [Coord xE y | y <- [yB .. yE - 1]]]

boundsRectSize (Coord xStart yStart, Coord xEnd yEnd)
    | xEnd < xStart = 0
    | yEnd < yStart = 0
    | otherwise = ((xEnd - xStart) * (yEnd - yStart))

-- all of the inside coords, none of the walls!
containedCoords :: BoundsRect -> [Coord]
containedCoords (Coord bX bY, Coord eX eY) = [Coord x y | x <- [bX..eX], y <- [bY..eY]]
