{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TypeSynonymInstances, RankNTypes, FlexibleContexts, OverlappingInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module SaveLoad (saveToFile, loadGameFromFile, loadDataFromFile) where

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
import Data.YamlObject (makeYaml, unmakeYaml)

saveToFile name content = BS.writeFile name $ encode $ makeYaml content


-- | loads a value of a given type from a JSON file. NB: Must be called with a monomorphic type! (for instance, (loadDataFromFile "foos.json" :: IO (Either String [Foo])))
loadDataFromFile :: forall a. FromYaml a => FilePath -> IO (Either String a)
loadDataFromFile name = do
  content <- BS.readFile name
  case decode content of
    Nothing -> return $ Left $ ("error parsing yaml for data file " ++ show name)
    Just yaml -> case (unmakeYaml yaml) of
                    Left e -> return $ Left $ show e
                    Right game -> return $ Right (game :: a)

loadGameFromFile name = (loadDataFromFile name) :: IO (Either String GameState)
