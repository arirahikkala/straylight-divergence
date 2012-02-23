{-# LANGUAGE TemplateHaskell, NamedFieldPuns, NoMonomorphismRestriction, BangPatterns, ViewPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module Object where

import BasicTypes

import Data.Accessor.Template
import Data.Typeable
--import Data.Generics.SYB.WithClass
import Data.Generics.SYB.WithClass.Derive
import Data.YamlObject (DoShare (..))

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
    , focus_ :: [ObjRef]
    , numFocuses_ :: ! Int
    } | 
    RangedWeapon {
      rangedWeaponPrototype_ :: RangedWeaponPrototype
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
    deriving (Show, Eq)

data RangedWeaponPrototype = RangedWeaponPrototype {
      rangedWeaponName :: String
    , rangedWeaponDamage :: Int
} deriving (Show, Eq)

data FurniturePrototype = FurniturePrototype {
      furniturePrototypeName :: String 
    , furniturePrototypeGlyph :: StyledChar
    , furniturePrototypeDamageSequence :: [(Double, RubbleMaterial)]
    , furniturePrototypeWeight :: Double
    , furniturePrototypeWalkable :: Bool
    , furniturePrototypeConcealment :: Double
} deriving (Show, Read, Eq)

instance DoShare FurniturePrototype where
    doShare = const True

data RubbleMaterial = WoodRubble | BookRubble | StoneRubble
                      deriving (Show, Read, Eq)

name (SpecificObject { obj = o }) = return $
    case o of
      (Door {}) -> "door"
      (DebugChar {char_}) -> "debug " ++ [char_]
      (RangedWeapon {rangedWeaponPrototype_ = proto} ) -> rangedWeaponName proto
      (Actor {}) -> "actor"
      (Furniture {furniturePrototype_ = proto}) -> furniturePrototypeName proto
      (Rubble { rubbleMaterial_ = material }) -> show material
      
--name (Door {}) = "door"
--name 

debugChar c = DebugChar c

door closed = Door closed

buddy = Actor (AIState []) 10 [] 1

data AIState = AIState {
      movementPlan_ :: [Coord]
} deriving (Show, Read, Eq)

$(derive [''Object, ''FurniturePrototype, ''RangedWeaponPrototype, ''RubbleMaterial, ''AIState])

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
                      , numFocuses_ = 1
                      , focus_ = []
                      }
