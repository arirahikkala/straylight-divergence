{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TemplateHaskell, PatternGuards #-}
module Action (Action (..), resolveAction) where

import BasicTypes
import CommonTypes
import Object
import Data.Data
import Game
import Data.Accessor
import Control.Monad (liftM, when)
import Util (whenM)
import qualified Data.Map as Map


data Action =
    Idle
    | WalkTo Coord
    | Attack { attackTarget :: ObjRef, attackWeapon :: ObjRef }
    | Quit
           deriving (Eq, Show)

--resolveAction :: SpecificObject -> Initiative -> Action -> PureGame (Maybe String)
resolveAction o i a = 
  gsGlobal references_ >>= \refs ->
  if not $ Map.member (ref o) refs
  then return Nothing
  else case a of
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
