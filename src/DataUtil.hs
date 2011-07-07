module DataUtil where

-- utilities for some basic data structures

import Data.Array.IArray

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

insertAssoc :: (Ord a, Ord k) => k -> a -> Map k (Set a) -> Map k (Set a)
insertAssoc k v =
    insertAssocSet k (Set.singleton v)

insertAssocSet :: (Ord a, Ord k) => k -> Set a -> Map k (Set a) -> Map k (Set a)
insertAssocSet k v =
    Map.insertWith' Set.union k v

removeAssoc k v =
    removeAssocSet k (Set.singleton v)

removeAssocSet k v =
    Map.alter (\s -> case s of 
                       Nothing -> Nothing
                       Just x -> let newX = Set.difference x v in
                                 if Set.null newX
                                 then Nothing
                                 else Just newX) k

lookupAssocSet :: Ord k => k -> Map k (Set a) -> Set a
lookupAssocSet k m = 
    case Map.lookup k m of
      Nothing -> Set.empty
      Just x -> x

copyArray a = listArray (bounds a) $ elems a

amapAny f a =
    listArray (bounds a) $ map f (elems a)
