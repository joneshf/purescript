{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.PureScript.Make
  (
  -- * Make API
    RebuildPolicy(..)
  , ProgressMessage(..), renderProgressMessage
  , MakeActions(..)
  , Externs()
  , rebuildModule
  , make

  -- * Implementation of Make API using files on disk
  , Make(..)
  , runMake
  , makeIO
  , readTextFile
  , buildMakeActions
  , inferForeignModules
  , inferForeignModules'
  ) where

import           Prelude.Compat

import           Control.Concurrent.Lifted as C
import           Control.Monad hiding (sequence)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Except
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Aeson (encode, decode)
import           Data.Function (on)
import           Data.Foldable (for_)
import           Data.List (foldl', sortBy, groupBy)
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Monoid ((<>))
import           Data.Time.Clock
import           Data.Traversable (for)
import           Data.Version (showVersion)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Language.PureScript.AST
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Linter
import           Language.PureScript.ModuleDependencies
import           Language.PureScript.Names
import           Language.PureScript.Options
import           Language.PureScript.Pretty.Erl
import           Language.PureScript.Renamer
import           Language.PureScript.Sugar
import           Language.PureScript.TypeChecker

import qualified Language.PureScript.CodeGen.Erl as E
import Language.PureScript.CodeGen.Erl.Common (atomModuleName, ModuleType(..))
import Language.PureScript.Parser.Erl

import qualified Language.PureScript.CoreFn as CF
import qualified Paths_purescript as Paths
import           System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing)
import           System.FilePath ((</>), takeDirectory, replaceExtension)
import           System.IO.Error (tryIOError)

-- | Progress messages from the make process
data ProgressMessage
  = CompilingModule ModuleName
  -- ^ Compilation started for the specified module
  deriving (Show, Eq, Ord)

-- | Render a progress message
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) = "Compiling " ++ T.unpack (runModuleName mn)

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (JavaScript, C++11, etc.)
--
-- * The details of how files are read/written etc.
data MakeActions m = MakeActions
  { getInputTimestamp :: ModuleName -> m (Either RebuildPolicy (Maybe UTCTime))
  -- ^ Get the timestamp for the input file(s) for a module. If there are multiple
  -- files (@.purs@ and foreign files, for example) the timestamp should be for
  -- the most recently modified file.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or 'Nothing' if any of the required
  -- output files are missing.
  , readExterns :: ModuleName -> m (FilePath, Externs)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
  }

-- | Generated code for an externs file.
type Externs = B.ByteString

-- | Determines when to rebuild a module
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways deriving (Show, Eq, Ord)

-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule
  :: forall m
   . (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule MakeActions{..} externs m@(Module _ _ moduleName _ _) = do
  progress $ CompilingModule moduleName
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    [desugared] <- desugar externs [withPrim]
    runCheck' (emptyCheckState env) $ typeCheckModule desugared

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      [renamed] = renameInModules [corefn]
      exts = moduleToExternsFile mod' env'
  evalSupplyT nextVar' . codegen renamed env' . encode $ exts
  return exts

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file and an @externs.json@ file.
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
make :: forall m. (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [Module]
     -> m [ExternsFile]
make ma@MakeActions{..} ms = do
  checkModuleNamesAreUnique

  (sorted, graph) <- sortModules ms

  barriers <- zip (map getModuleName sorted) <$> replicateM (length ms) ((,) <$> C.newEmptyMVar <*> C.newEmptyMVar)

  for_ sorted $ \m -> fork $ do
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup (getModuleName m) graph)
    buildModule barriers (importPrim m) (deps `inOrderOf` map getModuleName sorted)

  -- Wait for all threads to complete, and collect errors.
  errors <- catMaybes <$> for barriers (takeMVar . snd . snd)

  -- All threads have completed, rethrow any caught errors.
  unless (null errors) $ throwError (mconcat errors)

  -- Bundle up all the externs and return them as an Environment
  (_, externs) <- unzip . fromMaybe (internalError "make: externs were missing but no errors reported.") . sequence <$> for barriers (takeMVar . fst . snd)
  return externs

  where
  checkModuleNamesAreUnique :: m ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates getModuleName ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName (head ms')
        in errorMessage $ DuplicateModule mn (map getModuleSourceSpan ms')

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [[a]]
  findDuplicates f xs =
    case filter ((> 1) . length) . groupBy ((==) `on` f) . sortBy (compare `on` f) $ xs of
      [] -> Nothing
      xss -> Just xss

  -- Sort a list so its elements appear in the same order as in another list.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: [(ModuleName, (C.MVar (Maybe (MultipleErrors, ExternsFile)), C.MVar (Maybe MultipleErrors)))] -> Module -> [ModuleName] -> m ()
  buildModule barriers m@(Module _ _ moduleName _ _) deps = flip catchError (markComplete Nothing . Just) $ do
    -- We need to wait for dependencies to be built, before checking if the current
    -- module should be rebuilt, so the first thing to do is to wait on the
    -- MVars for the module's dependencies.
    mexterns <- fmap unzip . sequence <$> traverse (readMVar . fst . fromMaybe (internalError "make: no barrier") . flip lookup barriers) deps

    case mexterns of
      Just (_, externs) -> do
        outputTimestamp <- getOutputTimestamp moduleName
        dependencyTimestamp <- maximumMaybe <$> traverse (fmap shouldExist . getOutputTimestamp) deps
        inputTimestamp <- getInputTimestamp moduleName

        let shouldRebuild = case (inputTimestamp, dependencyTimestamp, outputTimestamp) of
                              (Right (Just t1), Just t3, Just t2) -> t1 > t2 || t3 > t2
                              (Right (Just t1), Nothing, Just t2) -> t1 > t2
                              (Left RebuildNever, _, Just _) -> False
                              _ -> True

        let rebuild = do
              (exts, warnings) <- listen $ rebuildModule ma externs m
              markComplete (Just (warnings, exts)) Nothing

        if shouldRebuild
          then rebuild
          else do
            mexts <- decodeExterns . snd <$> readExterns moduleName
            case mexts of
              Just exts -> markComplete (Just (mempty, exts)) Nothing
              Nothing -> rebuild
      Nothing -> markComplete Nothing Nothing
    where
    markComplete :: Maybe (MultipleErrors, ExternsFile) -> Maybe MultipleErrors -> m ()
    markComplete externs errors = do
      putMVar (fst $ fromMaybe (internalError "make: no barrier") $ lookup moduleName barriers) externs
      putMVar (snd $ fromMaybe (internalError "make: no barrier") $ lookup moduleName barriers) errors

  maximumMaybe :: Ord a => [a] -> Maybe a
  maximumMaybe [] = Nothing
  maximumMaybe xs = Just $ maximum xs

  -- Make sure a dependency exists
  shouldExist :: Maybe UTCTime -> UTCTime
  shouldExist (Just t) = t
  shouldExist _ = internalError "make: dependency should already have been built."

  decodeExterns :: Externs -> Maybe ExternsFile
  decodeExterns bs = do
    externs <- decode bs
    guard $ T.unpack (efVersion externs) == showVersion Paths.version
    return externs

-- | A monad for running make actions
newtype Make a = Make
  { unMake :: ReaderT Options (ExceptT MultipleErrors (Logger MultipleErrors)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

instance MonadBase IO Make where
  liftBase = liftIO

instance MonadBaseControl IO Make where
  type StM Make a = Either MultipleErrors a
  liftBaseWith f = Make $ liftBaseWith $ \q -> f (q . unMake)
  restoreM = Make . restoreM

-- | Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
runMake :: Options -> Make a -> IO (Either MultipleErrors a, MultipleErrors)
runMake opts = runLogger' . runExceptT . flip runReaderT opts . unMake

-- | Run an 'IO' action in the 'Make' monad, by specifying how IO errors should
-- be rendered as 'ErrorMessage' values.
makeIO :: (IOError -> ErrorMessage) -> IO a -> Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . singleError . f) return e

-- | Read a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
readTextFile :: FilePath -> Make B.ByteString
readTextFile path = makeIO (const (ErrorMessage [] $ CannotReadFile path)) $ B.readFile path

inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules = inferForeignModules' "erl"

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules'
  :: forall m
   . MonadIO m
  => String
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules' suffix = fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path suffix
      exists <- liftIO $ doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing

-- | A set of make actions that read and write modules from the given directory.
buildMakeActions
  :: FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix =
    MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where

  getInputTimestamp :: ModuleName -> Make (Either RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverse getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    dumpCoreFn <- asks optionsDumpCoreFn
    let filePath = T.unpack $ runModuleName mn
        outputName = T.unpack $ atomModuleName mn PureScriptModule <> ".erl"
        erlFile = outputDir </> filePath </> outputName
        externsFile = outputDir </> filePath </> "externs.json"
        coreFnFile = outputDir </> filePath </> "corefn.json"
        min3 js exts coreFn
          | dumpCoreFn = min (min js exts) coreFn
          | otherwise = min js exts
    min3 <$> getTimestamp erlFile <*> getTimestamp externsFile <*> getTimestamp coreFnFile

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

    (exports, rawErl) <- E.moduleToErl env m foreignExports
    let pretty = prettyPrintErl rawErl
    let moduleName = runModuleName mn
        outFile = outputDir </> T.unpack moduleName </> T.unpack (atomModuleName mn PureScriptModule) ++ ".erl"
        externsFile = outputDir </> T.unpack moduleName </> "externs.json"
        foreignFile = outputDir </> T.unpack moduleName </> T.unpack (atomModuleName mn ForeignModule) ++ ".erl"
        prefix :: [Text]
        prefix = ["Generated by psc version " <> T.pack (showVersion Paths.version) | usePrefix]
        directives :: [Text] 
        directives = [
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
