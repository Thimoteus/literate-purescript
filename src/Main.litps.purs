module Main where

import Prelude
import Control.Monad (when)
import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION())

import Data.Array (drop, head, filter, zip, concatMap)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String (dropWhile)

import Node.Path (FilePath(), dirname)
import Node.Encoding (Encoding(..))
import Node.FS (FS())
import Node.FS.Sync (readTextFile, writeTextFile, exists, stat, readdir, mkdir)
import Node.FS.Stats (isDirectory, isFile)
import Node.Process (argv)

import Literate.Parser (transpile)
type RecursivEff a = forall e. Eff ( fs :: FS, err :: EXCEPTION | e ) a
getDirFiles :: FilePath -> RecursivEff (Tuple (Array String) (Array String))
getDirFiles root = do
  contents <- map (\ x -> root <> "/" <> x) <$> readdir root
  stats <- traverse stat contents
  let files = map fst <<< filter snd <<< zip contents <<< map isFile $ stats
      dirs = map fst <<< filter snd <<< zip contents <<< map isDirectory $ stats
  pure $ Tuple dirs files
recursivelyGetDirFiles :: String -> RecursivEff (Tuple (Array String) (Array String))
recursivelyGetDirFiles root = do
  dirFiles <- getDirFiles root
  let dirs = fst dirFiles
      files = snd dirFiles
  s <- traverse recursivelyGetDirFiles dirs
  let newDirs = concatMap fst s
      newFiles = concatMap snd s
  pure $ Tuple (dirs <> newDirs) (files <> newFiles)
writeSingleFile :: FilePath -> String -> RecursivEff Unit
writeSingleFile path contents = do
  makeSurePathExists path
  writeTextFile UTF8 (path <> ".purs") (transpile contents)
makeSurePathExists :: FilePath -> RecursivEff Unit
makeSurePathExists path = do
  let dir = dirname path
  doesExist <- exists dir
  when (not doesExist) $ mkdir dir
readSingleFile :: FilePath -> RecursivEff String
readSingleFile path = readTextFile UTF8 path
main = void do
  folder <- fromMaybe "." <<< head <<< drop 2 <$> argv
  files <- snd <$> recursivelyGetDirFiles folder
  fileContents <- traverse readSingleFile files
  outputFolder <- exists "src/"
  when (not outputFolder) $ mkdir "src/"
  let pathContents = zip (map (("src/" <>) <<< dropWhile (/= '/')) $ files) fileContents
  traverse (\ x -> writeSingleFile (fst x) (snd x)) pathContents
