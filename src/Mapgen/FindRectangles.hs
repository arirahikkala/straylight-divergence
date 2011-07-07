module Mapgen.FindRectangles where

import Rect
import Mapgen.Rect
import BasicTypes

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (minimumBy)

import Data.AdditiveGroup ((^-^))
import Data.VectorSpace (magnitudeSq)
import Data.Ord (comparing)

import Control.Arrow ((&&&))

import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PSQ


-- Whitespace rectangle finder. Based on TM Breuel, Two Geometric Algorithms For
-- Layout Analysis, Workshop on Document Analysis Systems 2002, pp. 188-199. Or
-- if you're a normal person who just clicks on hyperlinks rather than track
-- references down by paper and all that,
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.17.5758&rep=rep1&type=pdf

findRectangles :: (BoundsRect)
               -> [BoundsRect]
               -> [BoundsRect]
findRectangles boundary obstacles =
    go (PSQ.singleton boundary (poorness boundary obstacles)) obstacles
        where
          go q os =
              case PSQ.minView q of
                Nothing -> []
                Just (r PSQ.:-> pri, newQ) ->
                    if pri /= poorness r os
                    then go (PSQ.insert r (poorness r os) newQ) os
                    else let pivot = pick r . within r $ os
                             clear = noObstacles r os
                             dirs = map (id &&& flip poorness os) $
                                    filter ((> 0) . boundsRectSize) $
                                    [clampNorth pivot r,
                                     clampSouth pivot r,
                                     clampWest pivot r,
                                     clampEast pivot r]
                          in if clear
                            then r : go newQ (r : os)
                            else go (foldr (uncurry PSQ.insert) newQ dirs) os

noObstacles r = all ((== 0) . boundsRectSize) . map (rectClamp r)


-- proly could come up with better sampling strategies than just taking a
-- couple of the first obstacles, but ehh
-- (fwiw just taking the first one always would probably be perfectly good enough and quite possibly not any worse than this)
pick r os =
    minimumBy (comparing (magnitudeSq .
                          (approximateCenter r ^-^) .
                          approximateCenter))
                  $ take 3 os

within r = filter ((>0) . boundsRectSize) . map (rectClamp r)

-- the smaller the poorness, the better!
poorness r os =
    Set.size (Set.fromList $ concatMap (rectCoords . rectClamp r) os)
    - boundsRectSize r
    - (uncurryCoord min $ uncurry (^-^) $ r)

uncurryCoord f (Coord x y) = f x y


rectClamp (Coord xBigStart yBigStart, Coord xBigEnd yBigEnd)
          (Coord xSmallStart ySmallStart, Coord xSmallEnd ySmallEnd) =
    (Coord (max xBigStart xSmallStart) (max yBigStart ySmallStart),
     Coord (min xBigEnd xSmallEnd) (min yBigEnd ySmallEnd))

clampNorth (Coord xBigStart yBigStart, Coord xBigEnd yBigEnd)
           (Coord xSmallStart ySmallStart, Coord xSmallEnd ySmallEnd) =
    (Coord xSmallStart (min ySmallStart yBigStart),
     Coord xSmallEnd (min ySmallEnd yBigStart))

clampSouth (Coord xBigStart yBigStart, Coord xBigEnd yBigEnd)
           (Coord xSmallStart ySmallStart, Coord xSmallEnd ySmallEnd) =
    (Coord xSmallStart (max ySmallStart yBigEnd),
     Coord xSmallEnd (max ySmallEnd yBigEnd))

clampWest (Coord xBigStart yBigStart, Coord xBigEnd yBigEnd)
          (Coord xSmallStart ySmallStart, Coord xSmallEnd ySmallEnd) =
    (Coord (min xSmallStart xBigStart) ySmallStart,
     Coord (min xSmallEnd xBigStart) ySmallEnd)

clampEast (Coord xBigStart yBigStart, Coord xBigEnd yBigEnd)
          (Coord xSmallStart ySmallStart, Coord xSmallEnd ySmallEnd) =
    (Coord (max xSmallStart xBigEnd) ySmallStart,
     Coord (max xSmallEnd xBigEnd) ySmallEnd)
