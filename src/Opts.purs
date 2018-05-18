module Opts where

import Prelude

import Constants (helpText)
import Data.Maybe (Maybe(..))
import Node.Optlicative (Opt(..), Optlicative, Preferences, defaultPreferences, flag, optional, string, withDefault)
import Node.Path (FilePath)

type Options =
  { help :: Boolean
  , version :: Boolean
  , input :: FilePath
  , output :: FilePath
  , inputExt :: String
  , outputExt :: String
  , file :: Maybe String
  }

prefs :: Preferences Options
prefs = defaultPreferences {globalOpts = globalOpts, usage = Just helpText}

globalOpts :: Optlicative Options
globalOpts =
  { help: _
  , version: _
  , input: _
  , output: _
  , inputExt: _
  , outputExt: _
  , file: _
  }
  <$> flag "help" (Just 'h')
  <*> flag "version" (Just 'v')
  <*> withDefault "literate/" (string "input" Nothing)
  <*> withDefault "src/" (string "output" Nothing)
  <*> withDefault "md" (string "i-ext" Nothing)
  <*> withDefault "purs" (string "o-ext" Nothing)
  <*> optional (string "file" Nothing)

compileOpts :: Optlicative Options
compileOpts =
  { help: _
  , version: _
  , input: _
  , output: _
  , inputExt: _
  , outputExt: _
  , file: _
  }
  <$> flag "help" (Just 'h')
  <@> false
  <*> withDefault "literate/" (string "input" Nothing)
  <*> withDefault "src/" (string "output" Nothing)
  <*> withDefault "md" (string "i-ext" Nothing)
  <*> withDefault "purs" (string "o-ext" Nothing)
  <*> optional (string "file" Nothing)

type CmdOpts =
  ( compile :: Opt Options ()
  )

cmdOpts :: Record CmdOpts
cmdOpts =
  { compile: Opt compileOpts {}
  }