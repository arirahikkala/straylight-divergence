{-# LANGUAGE NamedFieldPuns #-}
module RenderObject where

import CommonTypes
import Object
import CursesWrap (ColorName (..), Style (..), StyledChar (..))

renderObject :: Monad m => SpecificObject -> GameT m StyledChar
renderObject s = 
    case obj s of
      (Actor {}) -> return $ StyledChar (Style True False Grey Black) '@'
      (Door {closed_ = c}) ->
          if c 
          then return $ StyledChar (Style False False Grey Black) '+'
          else return $ StyledChar (Style False False Grey Black) '-'
      (DebugChar {char_ = c}) -> return $ StyledChar (Style False False Grey Black) c
      (Rubble { rubbleMaterial_ = m}) ->
             return $ StyledChar (Style False False (rubbleColour s) Black) $ (rubbleCharacters !! (mod (fromEnum $ ref s) numRubbleCharacters))
      (Furniture { furniturePrototype_ = (FurniturePrototype { furniturePrototypeGlyph })}) ->
          return furniturePrototypeGlyph

rubbleColour = const $ Yellow

numRubbleCharacters = length rubbleCharacters
rubbleCharacters = ".+`'"

objectColor = const (Grey, Black)
