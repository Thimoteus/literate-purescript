module Literate.Parser where

import Prelude

import Data.List (List(), mapMaybe)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldMap)
import Data.String (split)

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Text.Parsing.Simple
import Text.Parsing.Combinators hiding (many, many1)

data Line = Comment String
          | Code String

instance showLine :: Show Line where
  show (Comment s) = "Comment " <> show s
  show (Code s) = "Code " <> show s
fromCode :: Line -> Maybe String
fromCode (Code s) = Just s
fromCode _ = Nothing

code :: Parser Line
code = ticked <|> indented

ticked :: Parser Line
ticked = bracket ticks goodStuff ticks

ticks :: Parser Unit
ticks = string "```" *> skipSpaces

goodStuff :: Parser Line
goodStuff = Code <<< fromCharList <$> many1 item <* notFollowedBy (string "```")

indented :: Parser Line
indented = Code <$> (exactly 4 space *> line)

birdTracks :: Parser Line
birdTracks = Code <$> (string "> " *> line)
line :: Parser String
line = fromCharList <$> many (sat (/= '\n')) <* skip newline <> eof
comment :: Parser Line
comment = Comment <$> (atMost 3 space *> line)
document :: Int -> Parser (List Line)
document n = atMost n (birdTracks <> comment)
renderCode :: Line -> String
renderCode (Code s) = s
renderCode _ = ""

renderProgram :: List Line -> String
renderProgram = foldMap (<> "\n") <<< mapMaybe fromCode
numberOfNewlines :: String -> Int
numberOfNewlines = length <<< split "\n"
transpile :: String -> String
transpile p = case renderProgram <$> parse (document $ numberOfNewlines p) p of
                      Just x -> x
                      Nothing -> "ERROR: Either your program has a syntax error in it, or I have a semantic error in me :("
