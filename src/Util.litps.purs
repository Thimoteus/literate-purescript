module Util where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Data.String (split, joinWith)

lines :: String -> Array String
lines = split "\n"

unlines :: Array String -> String
unlines = joinWith "\n"
