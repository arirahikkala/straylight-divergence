{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts, FlexibleInstances, DeriveGeneric #-}
module BasicTypes where

import Data.Typeable
import Data.Ix (Ix)
import Data.Set (Set)
import System.Random (Random, random, randomR)
import Data.AdditiveGroup
import Data.VectorSpace
import Test.QuickCheck
import Control.Exception
import GHC.Generics
import Data.YamlObject (ToYaml, FromYaml)

data Location = OnMap { level :: !LevelRef, coord :: !Coord } | InContainer !ObjRef
                deriving (Show, Eq, Ord, Read, Typeable, Generic) -- Ord instance used only for storing these in a Map

data Coord = Coord { x :: !Int, y :: !Int }
             deriving (Eq, Ord, Show, Ix, Read, Typeable, Generic)

newtype ObjRef = ObjRef Int
    deriving (Show, Eq, Ord, Enum, Read, Ix, Typeable, Generic)

newtype LevelRef = LevelRef Int
    deriving (Show, Eq, Ord, Enum, Read, Ix, Typeable, Generic)

instance ToYaml Coord
instance ToYaml ObjRef
instance ToYaml LevelRef
instance FromYaml Coord
instance FromYaml ObjRef
instance FromYaml LevelRef

type BoundsRect = (Coord, Coord)

instance Arbitrary Coord where
    arbitrary = 
        do x <- arbitrary
           y <- arbitrary
           return $ Coord x y
    shrink (Coord x y) = 
        [Coord xs ys | xs <- shrink x, ys <- shrink y]

instance AdditiveGroup Coord where
    zeroV = Coord 0 0
    (Coord x1 y1) ^+^ (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)
    negateV (Coord x y) = Coord (negate x) (negate y)

instance VectorSpace Coord where
    type Scalar Coord = Double
    v *^ (Coord x y) = Coord (round (fromIntegral x * v)) (round (fromIntegral y * v))

instance InnerSpace Coord where
    (Coord x1 y1) <.> (Coord x2 y2) = fromIntegral (x1 * x2 + y1 * y2)

instance Random Coord where
    random g = let (x, g1) = random g
                   (y, g2) = random g1 in
               (Coord x y, g2)
    randomR (Coord x1 y1, Coord x2 y2) g =
        let (x, g1) = randomR (x1, x2) g
            (y, g2) = randomR (y1, y2) g1 in
        (Coord x y, g2)


type RefSet = Set ObjRef

data GameException = DataFormatException String
                     deriving (Show, Typeable)

instance Exception GameException 

