{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module SaveLoad (saveToFile) where

import Object
import BasicTypes
import CommonTypes
import Compound
import Tile
import LocationMap

import Data.YamlObject
import Text.YamlPickle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

saveToFile name content = TIO.writeFile name $ encode $ makeYaml content