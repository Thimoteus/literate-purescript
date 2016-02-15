module Literate.Parser where

import Prelude

import Data.List (List(), mapMaybe)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldMap, intercalate)
import Data.String (split, take)

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Text.Parsing.Simple
import Text.Parsing.Combinators hiding (many, many1)

data Line = Comment String
          | Code String
          | Empty

instance showLine :: Show Line where
  show (Comment s) = "Comment " <> show s
  show (Code s) = "Code " <> show s
  show Empty = ""
line :: Parser String
line = fromCharList <$> many1 (sat (/= '\n')) <* skip newline
emptyLine :: Parser Line
emptyLine = string "\n" *> pure Empty
fromCode :: Line -> Maybe String
fromCode (Code s) = Just s
fromCode _ = Nothing

code :: Parser Line
code = ticked <|> birdTracks <|> indented
ticked :: Parser Line
ticked = bracket ticks goodStuff ticks

ticks :: Parser Unit
ticks = string "```" *> skip (string "purescript") *> skipSpaces

goodStuff :: Parser Line
goodStuff = Code <<< intercalate "\n" <$> many noTickLine

noTickLine :: Parser String
noTickLine = (line |= \ l -> take 3 l /= "```") <> (string "\n" *> pure "")
indented :: Parser Line
indented = Code <$> (exactly 4 space *> line)
birdTracks :: Parser Line
birdTracks = Code <$> (string "> " *> line) --*
comment :: Parser Line
comment = Comment <$> (skipSpaces *> line)
document :: Parser (List Line)
document = many (emptyLine <> code <> comment)
renderCode :: Line -> String
renderCode (Code s) = s
renderCode _ = ""

renderProgram :: List Line -> String
renderProgram = foldMap (<> "\n") <<< mapMaybe fromCode
numberOfNewlines :: String -> Int
numberOfNewlines = length <<< split "\n"
transpile :: String -> String
transpile p = case renderProgram <$> parse document p of
                   Just x -> x
                   Nothing -> "ERROR: The \"impossible\" happened."
