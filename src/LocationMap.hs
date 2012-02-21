{-# LANGUAGE BangPatterns, DeriveDataTypeable, TemplateHaskell, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module LocationMap where

import DataUtil
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Data.Typeable
import BasicTypes
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set (empty)

{- Note: Since every value in one side of a LocationMap is a key in the other 
   side, MapS are spine-strict, and the LocationMap constructor is strict in 
   both of its members, LocationMapS *should* be strict for all
   keys and values. At least as far as I can tell.
-}
data LocationMap r l = 
    LocationMap { mr :: !(Map r l),
                  ml :: !(Map l (Set r))} deriving (Show, Read)

$(derive [''LocationMap])
-- A slightly faster version of insert for placing a given object into the map for the first time
introduce :: (Ord r, Ord l) => r -> l -> LocationMap r l -> LocationMap r l
introduce r l (LocationMap mr ml) = LocationMap (Map.insert r l mr) (insertAssoc l r ml)

insert :: (Ord r, Ord l) => r -> l -> LocationMap r l -> LocationMap r l
insert r l (LocationMap mr ml) = 
    let oldL = Map.lookup r mr in
    LocationMap (Map.insert r l mr)
                (insertAssoc l r $
                 case oldL of
                   Nothing -> ml
                   Just jOldL ->
                       removeAssoc jOldL r $ ml)

delete :: (Ord r, Ord l) => r -> LocationMap r l -> LocationMap r l
delete r (LocationMap mr ml) =
    let l = Map.lookup r mr in
    LocationMap (Map.delete r mr)
                (case l of
                   Nothing -> ml
                   Just jL -> removeAssoc jL r $ ml)

moveFrom :: (Ord r, Ord l) => r -> l -> l -> LocationMap r l -> LocationMap r l
moveFrom r oldL newL (LocationMap mr ml) =
    LocationMap (Map.insert r newL mr)
                (insertAssoc newL r $
                 removeAssoc oldL r ml)

move :: (Ord r, Ord l) => r -> l -> LocationMap r l -> LocationMap r l
move r l (LocationMap mr ml) =
    let oldL = Map.lookup r mr in
    LocationMap (Map.insert r l mr)
                (insertAssoc l r $
                 case oldL of
                   Nothing -> ml
                   Just jOldL -> removeAssoc jOldL r $ ml)

lookupLoc :: (Ord r, Ord l) => r -> LocationMap r l -> Maybe l
lookupLoc r (LocationMap mr _) = Map.lookup r mr

lookupObjRefs :: (Ord r, Ord l) => l -> LocationMap r l -> Set r
lookupObjRefs l (LocationMap _ ml) = 
    lookupAssocSet l ml

