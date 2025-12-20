{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

module CommandLine
  ( LogMode(..),Config(..),BuildMode(..),CacheDirSpec(..)
  , exec
  ) where

import Control.Monad (when)
import Data.List (intercalate)
import Locate (Dir,makeAbsoluteDir)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import Options.Applicative -- fully opened

data LogMode = LogQuiet | LogNormal | LogActions

data Config = Config
  { startDir :: Dir
  , homeDir :: Dir
  , cacheDir :: Dir
  , worker :: Bool
  , buildMode :: BuildMode
  , args :: [FilePath]
  , jnum :: Int
  , seePid :: Bool
  , cacheDirSpec :: CacheDirSpec
  , logMode :: LogMode
  , debugDemand :: Bool
  , debugExternal :: Bool
  , debugInternal :: Bool
  , debugLocking :: Bool
  , withPromotion :: Bool
  , reportRelative :: Bool
  , flagQ :: Bool
  , flagA :: Bool
  }

data CacheDirSpec = CacheDirDefault | CacheDirChosen String | CacheDirTemp

data BuildMode
  = ModeBuild
  | ModeTest
  | ModeRun
  | ModeCat
  | ModeExec
  | ModeInstall
  | ModeListTargets
  | ModeListRules
  deriving (Bounded,Enum)

instance Show BuildMode where
  show = \case
    ModeBuild -> "build"
    ModeCat -> "cat"
    ModeExec -> "exec"
    ModeInstall -> "install"
    ModeRun -> "run"
    ModeTest -> "test"
    ModeListTargets -> "list-targets"
    ModeListRules -> "list-rules"

modeHelp :: String
modeHelp = intercalate "|" (map show all)
  where all :: [BuildMode] = [minBound ..maxBound]

instance Read BuildMode where
  readsPrec _ = \given -> [ (x,"") | x <- all, show x == given ]
    where all = [minBound ..maxBound]

exec :: IO Config
exec = do

  homeDir <- makeAbsoluteDir <$> maybe "/tmp" id <$> lookupEnv "HOME"
  startDir <- makeAbsoluteDir <$> getCurrentDirectory
  let cacheDir = undefined -- TODO: ahem!
  let worker = False

  let
    positive :: ReadM Int  = do
      i <- auto
      when (i < 1) $
        readerError "Value must be at least 1"
      pure i

  let
    confParser :: Parser Config
    confParser = do

      let logMode = undefined -- TODO kill

      buildMode <- argument auto $
        metavar "MODE" <>
        help modeHelp

      args <- many $ strArgument $
        metavar "ARG" <>
        help "Target or directory"

      jnum <- option positive $
        hidden <>
        short 'j' <>
        long "jobs" <>
        metavar "NUM" <>
        value 3 <>
        showDefault <>
        help "Allow NUM jobs in parallel"

      flagQ <- switch $
        hidden <>
        short 'q' <>
        long "quiet" <>
        help "Build quietly, except for errors"

      flagA <- switch $
        hidden <>
        short 'a' <>
        long "show-actions" <>
        help "Log rule actions when executed"

      debugDemand <- switch $
        hidden <>
        short 'd' <>
        long "show-demand" <>
        help "Log build targets when demanded"

      seePid <- switch $
        hidden <>
        short 'p' <>
        long "show-pid" <>
        help "Prefix log lines with pid"

      cacheDirSpec <-
        CacheDirChosen <$>
        (strOption $
         hidden <>
         short 'c' <>
         long "cache" <>
         metavar "DIR" <>
         help "Use .cache/jenga in DIR instead of $HOME"
        ) <|>
        (flag' CacheDirTemp $
         hidden <>
         short 'f' <>
--         long "temporary-cache" <>
         long "force" <>
         help "Build using temporary cache to force build actions"
        ) <|>
        pure CacheDirDefault

      reportRelative <- switch $
        hidden <>
        long "rel" <>
        help "Report paths relative to starting directory"

      withPromotion <- switch $
        hidden <>
        long "promote" <>
        help "Promote test output to .expected files"

      debugExternal <- switch $
        hidden <>
        long "debug-external" <>
        help "Debug calls to md5sum"

      debugInternal <- switch $
        hidden <>
        long "debug-internal" <>
        help "Debug file system access"

      debugLocking <- switch $
        hidden <>
        long "debug-locking" <>
        help "Debug locking behaviour"

      pure $ Config {..}

  let
    myPrefs :: ParserPrefs
    myPrefs = prefs $
      showHelpOnError <>
      showHelpOnEmpty <>
      subparserInline <>
      mempty
  let
    im :: InfoMod Config
    im =
      header "jenga: A build system"
--      <> footer "JENGA-FOOTER" -- TODO: link to github?
--      <> progDesc "JENGA-DESC" -- TODO: description of modes?

  let
    pic :: ParserInfo Config
    pic = info (helper <*> confParser) im

  customExecParser myPrefs pic
