module Utils where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)


parseYear :: String -> Maybe Int
parseYear dateString =
  case splitOn "-" dateString of
    [year, _, _] -> readMaybe year
    _            -> Nothing
