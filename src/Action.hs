{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TemplateHaskell #-}
module Action (Action (..), resolveAction) where

import BasicTypes
import CommonTypes
import Object
import Data.Data
import Game
import Data.Generics.SYB.WithClass.Derive
import Data.Accessor
import Control.Monad (liftM)
import Util (whenM)
import qualified Data.Map as Map


data Action =
    Idle
    | WalkTo Coord
    | Attack { attackTarget :: ObjRef, attackWeapon :: ObjRef }
    | Quit
           deriving (Eq, Show)

$(derive [''Action])
--resolveAction :: SpecificObject -> Initiative -> Action -> PureGame (Maybe String)
resolveAction o i a = 
    case a of
      Idle -> return Nothing
      WalkTo c -> do pos <- mapPosition (ref o)
                     tryWalkTo o (fst pos, c)
      Attack target weapon ->
          do wo <- dereferObj weapon
             case wo of
               RangedWeapon { rangedWeaponPrototype_ = prot } -> do
                       mGlobal (references ^: Map.adjust (health ^: subtract (rangedWeaponDamage prot)) target)
                       mpr "Bang!"
                       whenM ((<0) `liftM` health_ `liftM` dereferObj target)
                                 $ removeActor target
                       return Nothing
