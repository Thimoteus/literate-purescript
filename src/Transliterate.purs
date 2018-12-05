module Transliterate where

import Prelude

import Control.Monad.Transformerless.State as State
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.String as String

type Markdown = String
type Purs = String

lines :: String -> Array String
lines = String.split (String.Pattern "\n")

unlines :: Array String -> String
unlines = intercalate "\n"

bracketsCode :: String -> Boolean
bracketsCode s = String.take 3 s == "```"

data TextState = Comment | Code

type State = {textState :: TextState, insertNl :: Boolean}

flipTextState :: State -> State
flipTextState s = s {textState = f s.textState } where
  f = case _ of
    Comment -> Code
    Code -> Comment

flipNl :: State -> State
flipNl o = o {insertNl = not o.insertNl}

step :: Array Purs -> Markdown -> State.State State (Array Purs)
step acc s | bracketsCode s = do
  {insertNl} <- State.get
  State.modify (flipTextState <<< flipNl)
  if insertNl
    then pure (acc <> ["\n"])
    else pure acc
step acc s = do
  {textState} <- State.get
  pure case textState of
    Comment -> acc
    Code -> acc <> [s]

initState :: State
initState = {textState: Comment, insertNl: false}

parse :: Array Markdown -> State.State State (Array Purs)
parse = Array.foldRecM step []

transliterate :: Markdown -> Purs
transliterate s =
  let
    ls = lines s
    parseResult = parse ls
    runResult = State.evalState parseResult initState
  in
    unlines runResult
