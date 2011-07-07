{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Compound where

import Data.Array.IArray
import BasicTypes
import Data.Map (Map)
import qualified Data.Map as Map
import Rect
import Data.Data
import Data.Typeable

import Util (newIArray)

-- Here be functions for managing data related to compounds (sets of rooms which are connected and which have some gameplay significance, including that the "normal" AI will prefer to stay inside a compound, and the mapgen prefers not to make doors between compounds; perhaps more to come).

-- almost the same deal with data structures as with objects: one structure for relating locations to compound IDs, another relating compound IDs to compounds
-- except here most coords are related to a compound (the outside is CompoundRef 0, so sort of a compound in itself) and at most one compound, hence the array
newtype CompoundRef = CompoundRef Int deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)

data Compounds = Compounds {
      compoundsCoords_ :: Array Coord CompoundRef
    , compoundsRefs_ :: Map CompoundRef Compound
} deriving (Show, Read, Data, Typeable)

data Compound = Compound {
      compoundRooms_ :: [BoundsRect]
} deriving (Show, Read, Data, Typeable)

--numberCompounds :: [[BoundsRect]] -> [(CompoundRef, Compound)]
numberCompounds cs =
    zip [CompoundRef 1..] $ map Compound cs


makeCompounds :: BoundsRect -> [[BoundsRect]] -> Compounds
makeCompounds bs rs = 
    let numbered = numberCompounds $ rs in
    Compounds (newIArray bs (CompoundRef 0) // concat [[(i, ref) | i <- concatMap rectCoords $ compoundRooms_ rect] | (ref, rect) <- numbered])
              (Map.fromList numbered)