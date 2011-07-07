module Mapgen.CampusArgs where

-- todo: It's gonna be fun splitting stuff up once I actually decide to add another mapgen...

data CampusArgs = CampusArgs {
      numOutsidePaths :: Int,
      numCrossingCorridors :: Int,
      numCorridors :: Int,
      randomConnectionChance :: Double,
      randomOutsideConnectionChance :: Double
}
