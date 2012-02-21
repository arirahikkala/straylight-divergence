{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module SaveLoad (saveToFile, loadGameFromFile) where

import Object
import BasicTypes
import CommonTypes
import Compound
import Tile
import LocationMap

import Data.YamlObject
import Text.YamlPickle
import Data.Array.Unboxed
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO

import qualified Data.ByteString as BS

saveToFile name content = BS.writeFile name $ encode $ makeYaml content
loadGameFromFile name = do
  content <- BS.readFile name
  case decode content of
    Nothing -> return $ Left $ "error parsing yaml for saved game (don't know yet how to actually pass the error upward cleanly)"
    Just yaml -> case (unmakeYaml yaml) :: Either FromYamlException GameState of
                    Left e -> return $ Left $ show e
                    Right game -> return $ Right game