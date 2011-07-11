module DataFile (readDataFile) where

import Data.List (isPrefixOf)

readDataFile :: FilePath -> IO String
readDataFile p =
    do x <- readFile p
       return $ unlines $ filter (not . isPrefixOf "--") $ lines x
