{-# LANGUAGE DeriveDataTypeable #-}
module Tile where
{-module Tile (Tile (), Floor (..), Wall (..), 
             makeWall, makeFloor, emptySpace, 
             tileColor, renderTile, describeTile, tileWallType,
             tileBlocksVision, tileBlocksMovement, tileHasFloor) where
-}
import Data.Bits
import CursesWrap (ColorName (..), StyledChar (..), Style (..))
import Data.Data
import Data.Typeable

-- bit allocation for now: last 4 bits for Floor, next 4 bits after them for Wall
-- I'm assuming Liberally that the Int type will have enough bits for my needs so I don't have to bother with casting :>
-- (in fact this file is pretty lazily written in general...)
newtype Tile = Tile {unTile :: Int} deriving (Show, Read, Data, Typeable)

data Floor = NoFloor | Concrete | Asphalt | Grass | Sett | Floor | Carpeting | Sand | Water deriving (Show, Eq, Enum)

data Wall = NoWall | PlainWall | GlassWall deriving (Eq, Enum, Show)

tileWallType :: Tile -> Wall
tileWallType (Tile t) = toEnum $ shiftR t 4

makeWall :: Wall -> Tile
makeWall w = Tile (shiftL (fromEnum w) 4)

makeFloor :: Floor -> Tile
makeFloor f = Tile (fromEnum f)

emptySpace :: Tile
emptySpace = Tile 0

wallBlocksVision NoWall = False
wallBlocksVision PlainWall = True
wallBlocksVision GlassWall = False

tileColor :: Tile -> ColorName
tileColor (Tile t) = go where
    go | PlainWall == (toEnum $ shiftR t 4) = Grey
       | GlassWall == (toEnum $ shiftR t 4) = Cyan
       | Concrete == toEnum t = Grey
       | Asphalt == toEnum t = Black
       | Grass == toEnum t = Green
       | Sett == toEnum t = Red
       | Floor == toEnum t = Black
       | Carpeting == toEnum t = Green
       | Sand == toEnum t = Yellow
       | Water == toEnum t = Blue
       | otherwise = error "tileColor: No colour defined for tile"

tileBlocksVision :: Tile -> Bool
tileBlocksVision (Tile t) = wallBlocksVision (toEnum $ shiftR t 4)

tileBlocksMovement :: Tile -> Bool
tileBlocksMovement (Tile t) = (shiftR t 4) > 0

tileHasFloor :: Tile -> Bool
tileHasFloor (Tile t) = t > 0


renderTile :: Tile -> StyledChar
renderTile t | tileBlocksMovement t = 
                 StyledChar (Style False False (tileColor t) Black) '#'
             | tileHasFloor t = 
                 StyledChar (Style False False (tileColor t) Black) '.'
             | otherwise = 
                 StyledChar (Style False False Grey Black) ' '

describeTile :: Tile -> String
describeTile t | tileBlocksMovement t = (show :: Wall -> String) (toEnum $ shiftR (unTile t) 4)
               | tileHasFloor t = (show :: Floor -> String) (toEnum $ unTile t)
               | otherwise = "Empty space"