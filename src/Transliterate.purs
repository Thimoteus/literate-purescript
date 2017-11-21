module Transliterate where

import Prelude
import Control.Monad.Transformerless.State as State
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.String as String

lines :: String -> Array String
lines = String.split (String.Pattern "\n")

unlines :: Array String -> String
unlines = intercalate "\n"

bracketsCode :: String -> Boolean
bracketsCode s = String.take 3 s == "```"

data State = Comment | Code

flipState :: State -> State
flipState Comment = Code
flipState Code = Comment

step :: Array String -> String -> State.State State (Array String)
step acc s | bracketsCode s = do
  State.modify flipState
  pure (acc <> ["\n"])
step acc s = do
  state <- State.get
  pure case state of
    Comment -> acc
    Code -> acc <> [s]

initState :: State
initState = Comment

parse :: Array String -> State.State State (Array String)
parse = Array.foldRecM step []

transliterate :: String -> String
transliterate s =
  let
    ls = lines s
    parseResult = parse ls
    runResult = State.evalState parseResult initState
  in
    unlines runResult
