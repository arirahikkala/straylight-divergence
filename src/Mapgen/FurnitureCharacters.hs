{-# LANGUAGE DeriveDataTypeable #-}
module Mapgen.FurnitureCharacters (FurnitureCharacters (..), CharacterDefinition (..), instantiateCharacter) where

import Data.List (find)
import Data.Typeable
import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Object

data FurnitureCharacters = FurnitureCharacters {
      charactersName :: String
    , charactersDefinitions :: [CharacterDefinition]
} deriving (Data, Typeable, Show, Read, Eq)

data CharacterDefinition = CharacterDefinition {
      cCharacter :: Char
    , cMeans :: String
} deriving (Data, Typeable, Show, Read, Eq)


-- will be a scary function, almost certainly off in its own file, and with a couple new arguments, once we're done with it
instantiateCharacter :: FurnitureCharacters -> Map String FurniturePrototype -> Char -> (Maybe Object, Bool)
instantiateCharacter (FurnitureCharacters {charactersDefinitions = cdefs }) prototypes c =
    case find ((==c) . cCharacter) cdefs of
      Nothing -> (Nothing, True)
      Just cdef -> case Map.lookup (cMeans cdef) prototypes of
                     Nothing -> (Nothing, True)
                     Just prot -> (Just $ Furniture prot, furniturePrototypeWalkable prot)
