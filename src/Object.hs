{-# LANGUAGE TemplateHaskell, NamedFieldPuns, NoMonomorphismRestriction, BangPatterns, DeriveDataTypeable, ViewPatterns #-}
module Object where

import BasicTypes

import Data.Accessor.Template
import Data.Data
import CursesWrap (StyledChar)
--import Data.Set (Set)
import qualified Data.Set as Set (empty)

data SpecificObject = SpecificObject { ref :: !ObjRef, obj :: Object }

instance Eq SpecificObject where 
    (SpecificObject {ref = a}) == (SpecificObject {ref = b}) = a == b

instance Ord SpecificObject where
   compare (SpecificObject {ref = a}) (SpecificObject {ref = b}) = compare a b

data Object = 
    Actor {
      ai_ :: ! AIState
    , health_ :: ! Int
    , stamina_ :: ! Int
    } | 
    Door { 
      closed_ :: ! Bool
    } |
    Furniture {
      furniturePrototype_ :: FurniturePrototype
    } |
    Rubble {
      rubbleMaterial_ :: RubbleMaterial
    } |
    DebugChar {
      char_ :: ! Char
    }
    deriving (Show, Read, Data, Typeable, Eq)

data FurniturePrototype = FurniturePrototype {
      furniturePrototypeName :: String 
    , furniturePrototypeGlyph :: StyledChar
    , furniturePrototypeDamageSequence :: [(Double, RubbleMaterial)]
    , furniturePrototypeWeight :: Double
    , furniturePrototypeWalkable :: Bool
    , furniturePrototypeConcealment :: Double
} deriving (Show, Read, Data, Typeable, Eq)

data RubbleMaterial = WoodRubble | BookRubble | StoneRubble
                      deriving (Show, Read, Data, Typeable, Eq)

--name (Door {}) = "door"
--name (DebugChar {char_}) = "debug " ++ [char_]

debugChar c = DebugChar c

door closed = Door closed

buddy = Actor (AIState []) 10 10

data AIState = AIState {
      movementPlan_ :: [Coord]
} deriving (Show, Read, Data, Typeable, Eq)

$( deriveAccessors ''Object )
$( deriveAccessors ''AIState )

takesActions (obj -> Actor {}) = return True
takesActions _ = return False

isAttackTargetable (obj -> Actor {}) = return True
isAttackTargetable _ = return False

isDoor (Door {}) = True
isDoor _ = False

isContainer (obj -> Actor {}) = return True
isContainer _ = return False

{-
mContents :: Object -> Maybe RefSet
mContents o | not (isContainer o) = Nothing
            | otherwise = Just (contents_ o)
-}
objBlocksVision (Door {closed_ = c}) = c
objBlocksVision _ = False

objBlocksWalking o =
    case o of
      Actor {} -> True
      Door {closed_ = c} -> c
      Furniture {furniturePrototype_ = FurniturePrototype { furniturePrototypeWalkable }} -> not furniturePrototypeWalkable
      _ -> False

--renderObject (Rubble {rubbleMaterial_}) = 
--    case rubbleMaterial_ of
      

renderSort (Actor {}) _ = LT
renderSort (Door {}) _ = GT
renderSort _ _ = EQ

objectRenderingOrder (Actor {}) = 0
objectRenderingOrder (Door {}) = 1
objectRenderingOrder _ = 2


defaultPlayer = Actor { ai_ = AIState []
                      , health_ = 10
                      , stamina_ = 10
                      }
