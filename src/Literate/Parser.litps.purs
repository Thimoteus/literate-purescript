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
code = ticked <|> indented <|> birdTracks
ticked :: Parser Line
ticked = bracket ticks goodStuff ticks
ticks :: Parser Unit
ticks = string "```" *> skipSpaces
goodStuff :: Parser Line
goodStuff = Code <<< (<> "\n") <<< intercalate "\n" <$> many noTickLine
indented :: Parser Line
indented = Code <$> (exactly 4 space *> line)
birdTracks :: Parser Line
birdTracks = Code <$> (string "> " *> line)
noTickLine :: Parser String
noTickLine = line |= \ l -> take 3 l /= "```"
comment :: Parser Line
comment = Comment <$> (skipSpaces *> line)
document :: Int -> Parser (List Line)
document n = atMost n (emptyLine <> birdTracks <> comment)
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
