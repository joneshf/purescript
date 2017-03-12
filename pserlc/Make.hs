{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Make where

import Prelude ()
import Prelude.Compat

import Control.Monad hiding (sequence)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class
import Control.Monad.Supply

import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Foldable (for_)
import Data.Version (showVersion)
import qualified Data.Map as M

import qualified Data.Text as T
import Data.Text (Text)

import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (tryIOError)

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Encoding as TE

import Language.PureScript.CodeGen.Erl as J
import Language.PureScript.CodeGen.Erl.Common (atomModuleName, ModuleType(..))
import Language.PureScript (Make, RebuildPolicy, ProgressMessage, Externs)
import Language.PureScript.Make (MakeActions(..), renderProgressMessage, readTextFile)

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Pretty
import Language.PureScript.Parser.Erl

import Paths_purescript as Paths

import Language.PureScript.CoreFn as CF

-- |
-- A set of make actions that read and write modules from the given directory.
--
buildMakeActions :: FilePath -- ^ the output directory
                 -> M.Map ModuleName (Either RebuildPolicy FilePath) -- ^ a map between module names and paths to the file containing the PureScript module
                 -> M.Map ModuleName FilePath -- ^ a map between module name and the file containing the foreign javascript for the module
                 -> Bool -- ^ Generate a prefix comment?
                 -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix =
  MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where

  getInputTimestamp :: ModuleName -> Make (Either RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverseEither getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1


  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = T.unpack $ runModuleName mn
        erlFile = outputDir </> filePath </> "index.erl"
        externsFile = outputDir </> filePath </> "externs.json"
    min <$> getTimestamp erlFile <*> getTimestamp externsFile

  readExterns :: ModuleName -> Make (FilePath, Externs)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> "externs.json"
    (path, ) <$> readTextFile path

  codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT Make ()
  codegen m env exts = do
    let mn = CF.moduleName m
    foreignExports <- lift $ case mn `M.lookup` foreigns of
      Just path
        | not $ requiresForeign m -> do
            tell $ errorMessage $ UnnecessaryFFIModule mn path
            return []
        | otherwise -> getForeigns path
      Nothing -> do
        when (requiresForeign m) $ throwError . errorMessage $ MissingFFIModule mn
        return []

    (exports, rawErl) <- J.moduleToErl env m foreignExports
    let pretty = prettyPrintErl rawErl
    let moduleName = runModuleName mn
        outFile = outputDir </> T.unpack moduleName </> T.unpack (atomModuleName mn PureScriptModule) ++ ".erl"
        externsFile = outputDir </> T.unpack moduleName </> "externs.json"
        foreignFile = outputDir </> T.unpack moduleName </> T.unpack (atomModuleName mn ForeignModule) ++ ".erl"
        prefix :: [Text] = ["Generated by psc version " <> T.pack (showVersion Paths.version) | usePrefix]
        directives :: [Text] = [
          "-module(" <> atomModuleName mn PureScriptModule <> ").",
          "-export([" <> T.intercalate ", " exports <> "]).",
          "-compile(nowarn_shadow_vars).",
          "-compile(nowarn_unused_vars).",  -- consider using _ vars
          "-compile(no_auto_import)."
          ]
    let erl :: Text = T.unlines $ map ("% " <>) prefix ++ directives ++ [ pretty ]
    lift $ do
      writeTextFile outFile $ B.fromStrict $ TE.encodeUtf8 erl
      for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)
      writeTextFile externsFile exts

  getForeigns :: String -> Make [(Text, Int)]
  getForeigns path = do
    text <- TE.decodeUtf8 . B.toStrict <$> readTextFile path
    pure $ either (const []) id $ parseFile path text

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (ErrorMessage [] $ CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists

  writeTextFile :: FilePath -> B.ByteString -> Make ()
  writeTextFile path text = makeIO (const (ErrorMessage [] $ CannotWriteFile path)) $ do
    mkdirp path
    B.writeFile path text
    where
    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage


-- Traverse (Either e) instance (base 4.7)
traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left x) = pure (Left x)
traverseEither f (Right y) = Right <$> f y


makeIO :: (IOError -> ErrorMessage) -> IO a -> Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . singleError . f) return e