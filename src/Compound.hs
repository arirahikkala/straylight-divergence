{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Compound where

import Data.Array.IArray
import BasicTypes
import CommonTypes
import Data.Map (Map)
import qualified Data.Map as Map
import Rect
import Data.Generics.SYB.WithClass.Derive
import Data.Typeable

import Util (newIArray)

-- Here be functions for managing data related to compounds (sets of rooms which are connected and which have some gameplay significance, including that the "normal" AI will prefer to stay inside a compound, and the mapgen prefers not to make doors between compounds; perhaps more to come).

-- almost the same deal with data structures as with objects: one structure for relating locations to compound IDs, another relating compound IDs to compounds
-- except here most coords are related to a compound (the outside is CompoundRef 0, so sort of a compound in itself) and at most one compound, hence the array

--numberCompounds :: [[BoundsRect]] -> [(CompoundRef, Compound)]
numberCompounds cs =
    zip [CompoundRef 1..] cs


makeCompounds :: BoundsRect -> [Compound] -> Compounds
makeCompounds bs rs = 
    let numbered = numberCompounds $ rs in
    Compounds (newIArray bs (CompoundRef 0) // concat [[(i, ref) | i <- concatMap rectCoords $ compoundRooms_ rect] | (ref, rect) <- numbered])
              (Map.fromList numbered)