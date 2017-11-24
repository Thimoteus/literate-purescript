module Main where

import Prelude

import Constants (helpCompile, helpText, versionText)
import Control.Monad.Aff.Console (log)
import Control.MonadZero (guard)
import Control.Parallel (parTraverse, parTraverse_)
import Data.Array (fold)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Maybe (Maybe(Just), isJust, maybe)
import Data.NonEmpty ((:|))
import Data.String as String
import Data.Tuple (fst, snd)
import Data.Validation.Semigroup (unV)
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.Optlicative (logErrors, optlicate)
import Node.Path (FilePath)
import Opts (Options, cmdOpts, prefs)
import Snail (Script, Snail, cat, crawl, exists, exitWith, file, folder, mkdir, run, (+>), (~?>))
import Transliterate (transliterate)

type App = Snail ()

type FileInfo =
  { content :: String
  , path :: FilePath
  -- , modified :: DateTime
  }

type FolderContents =
  { folders :: Array FilePath
  , others :: Array FileInfo
  }

writeContentToSource :: FileInfo -> App Unit
writeContentToSource v = v.content +> file v.path

rewriteRoot :: String -> FilePath -> FilePath
rewriteRoot root f = root <> String.dropWhile (_ /= '/') f

rewriteExt :: String -> FilePath -> FilePath
rewriteExt ext f
  | String.contains (String.Pattern ".") f =
    reverseString <<<
    (reverseString ext <> _) <<< 
    String.dropWhile (_ /= '.') <<<
    reverseString $ f
      where
        reverseString =
          String.fromCharArray <<< Array.reverse <<< String.toCharArray
  | otherwise = f <> "." <> ext

prepareSourceFileInfo :: String -> FilePath -> FileInfo -> FileInfo
prepareSourceFileInfo ext root o = o
  { path = rewriteRoot root (rewriteExt ext o.path)
  , content = transliterate o.content }

getFolderContents :: FilePath -> App FolderContents
getFolderContents f = do
  contents <- map (f <> _) <$> readdir f
  stats <- parTraverse stat contents
  let
    zipped = Array.zip contents stats
    split = Array.partition (isDirectory <<< snd) zipped
    folders = (_ <> "/") <<< fst <$> split.yes
    notfolders = fst <$> split.no
  texts <- parTraverse (cat <<< file) notfolders
  let others = Array.zipWith {content: _, path: _} texts notfolders
  pure {folders, others}

recursivelyGetContents :: FilePath -> App FolderContents
recursivelyGetContents f = do
  immediate <- getFolderContents f
  nested <- parTraverse recursivelyGetContents immediate.folders
  let
    folders = immediate.folders <> fold (_.folders <$> nested)
    others = immediate.others <> fold (_.others <$> nested)
  pure {folders, others}

getDirsToCreate :: FilePath -> Array FilePath -> Array FileInfo -> Array FilePath
getDirsToCreate root all toXlit = Array.sortBy depth do
  fldr <- all
  guard $
    Array.any
      (\ fl -> String.contains (String.Pattern fldr) fl.path)
      toXlit
  pure (rewriteRoot root fldr)
  where
    depth = compare `on` numberOfSlashes
    numberOfSlashes = Array.length <<< Array.filter isSlash <<< String.toCharArray
    isSlash = (_ == '/')

runRoot :: Options -> App Unit
runRoot o = do
  when o.help (exitWith 0 helpText)
  when o.version (exitWith 0 versionText)
  maybe (pure unit) (runSingle o.outputExt) o.file
  contents <- recursivelyGetContents o.input
  let
    toTranslit = Array.filter translittable contents.others
    toMkdir = getDirsToCreate o.output contents.folders toTranslit
    translittable {path} =
      isJust $ String.stripSuffix (String.Pattern ("." <> o.inputExt)) path
    newFiles =
      map (prepareSourceFileInfo o.outputExt o.output) toTranslit
  log $
    "Transliterating " <> show (Array.length toTranslit) <> " file(s) from " <>
    o.input <> " to " <> o.output <> " ..."
  exists (folder o.output) ~?> \ _ ->
    exitWith 1 ("Folder " <> o.output <> " exists, aborting")
  traverse_ (mkdir <<< folder) $ [o.output] <> toMkdir
  parTraverse_ (\ x -> x.content +> file x.path) newFiles
  log "Done."

runSingle :: String -> FilePath -> App Unit
runSingle ext f = do
  log $ "Transliterating " <> f <> " ..."
  contents <- transliterate <$> cat (file f)
  contents +> file (rewriteExt ext f)
  exitWith 0 "Done."

runCompile :: Options -> App Unit
runCompile o = do
  when o.help (exitWith 0 helpCompile)
  runRoot o
  run $ "pulp" :| ["build", "--src-path", o.output]

main :: Script () Unit
main = do
  {cmd, value} <- optlicate cmdOpts prefs
  case cmd of
    Just "compile" -> unV logErrors (crawl <<< runCompile) value
    _ -> unV logErrors (crawl <<< runRoot) value
