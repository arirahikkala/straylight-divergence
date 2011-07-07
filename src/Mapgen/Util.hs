{-# LANGUAGE NoMonomorphismRestriction #-}
module Mapgen.Util where

import Data.List (intercalate)
import Data.Array.IArray
import Control.Arrow ((&&&), (***))
import Data.AdditiveGroup ((^+^), (^-^))

import Util (array2dToLists)
import Tile

import BasicTypes
import Mapgen.Rect (wallTiles)
import Data.Graph.Inductive (lab)

lastN n = reverse . take n . reverse
padLeft padding width xs = replicate (width - length xs) padding ++ xs


showCoordIntArrayN n = intercalate "\n" . map (intercalate " " . map (padLeft '0' n . lastN n . show)) . array2dToLists

showCoordIntArray = intercalate "\n" . map (map (last . show)) . array2dToLists 


addEdgeWalls bounds w =
    w // map (id &&& const (makeWall PlainWall)) 
          (wallTiles $ ((^+^ Coord 1 1) *** id) bounds)


floorEverything =
    amap (const $ makeFloor Grass)

justLab g e = 
    case lab g e of
      Nothing -> error "justLab: Unjustified use of justLab!"
      Just x -> x
