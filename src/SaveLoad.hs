{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module SaveLoad (serialize, deserialize, StoredState ()) where

import Object
import BasicTypes
import CommonTypes
import Compound
import Tile
import LocationMap

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Arrow

{- Okay so here's the deal. I have one very simple need: Sharing-preserving serialisation. There are libraries for that on Hackage. They all blow. At least, all the ones I found did. Some of them have no tutorial documentation so it's bloody difficult to figure them out in the first place, some of them are completely undocumented, some are bitrotted, etc.. Some use generics, in which case (at least based on what happened to json) they'll choke on the "helpfully" impoverished Data instances for Array and Set or etc.. Some (most) *don't* use generics, meaning that I'd have to tediously write picklers for everything I put in the game state.

This is the intermediate approach I ended up going with: Just use Haskell's Read/Show, which are easy and reliable (even if those parsing errors sure *could* be more helpful) and, most importantly, actually work for any data type, and write a "StoredState" hierarchy of types. Or actually just making a single StoredState type will probably make it easier because then I can just have a typeclass for the conversion and can just toss new types in at any time without worrying where they are in the structure.

-}

class GameSerialize a where
    serialize :: a -> StoredState
    deserialize :: StoredState -> Reader GameConfig a

data StoredState = 
-- short names for the really common constructors
    C (Int, Int)  -- Coord
  | T Int -- Tile
-- everything above anything we want to preserve sharing on gets a custom constructor
  | SGame Levels Positions [(ObjRef, StoredState)] ObjRef Time
  | SFurniture String
-- everything else we just wrap up trivially
  | SObject Object
  | SLevel Level
  | SCompounds Compounds
  | SLocation Location
  | SObjRef ObjRef
  | SLevelRef LevelRef
  | SFacing Facing
    deriving (Show, Read)

instance GameSerialize Coord where
    serialize (Coord x y) = C (x, y)
    deserialize (C (x, y)) = return (Coord x y)

instance GameSerialize Tile where
    serialize (Tile i) = T i
    deserialize (T i) = return (Tile i)

instance GameSerialize GameState where
    serialize (GameState { levels_, positions_, references_, firstUnusedId_, currentTurn_ }) =
        SGame levels_
              positions_
              (map (second serialize) $ Map.toList references_)
              firstUnusedId_
              currentTurn_
    deserialize (SGame levels positions references firstUnusedId currentTurn) =
        do refs <- mapM deserialize $ map snd references
           return $
             GameState levels
                       positions
                       (Map.fromList $ zip (map fst references) refs)
                       firstUnusedId
                       currentTurn
                       []
                       False
                       False

instance GameSerialize Object where
    serialize (Furniture {furniturePrototype_ = (FurniturePrototype {furniturePrototypeName})}) = SFurniture furniturePrototypeName
    serialize a = SObject a
    deserialize (SFurniture prototype) = 
        do prototypes <- asks furniturePrototypes_
           case Map.lookup prototype prototypes of
             Nothing -> error ("no furniture prototype for " ++ prototype)
             Just x -> return $ Furniture x
    deserialize (SObject a) = return $ a

instance GameSerialize Level where
    serialize a = SLevel a
    deserialize (SLevel a) = return $ a

instance GameSerialize Compounds where
    serialize a = SCompounds a
    deserialize (SCompounds a) = return $ a

instance GameSerialize Location where
    serialize a = SLocation a
    deserialize (SLocation a) = return $ a

instance GameSerialize LevelRef where
    serialize a = SLevelRef a
    deserialize (SLevelRef a) = return $ a

instance GameSerialize ObjRef where
    serialize a = SObjRef a
    deserialize (SObjRef a) = return $ a

{-
instance GameSerialize aoeu where
    serialize a = Saoeu a
    deserialize (Saoeu a) = return $ a
-}

instance GameSerialize Facing where
    serialize a = SFacing a
    deserialize (SFacing a) = return $ a
