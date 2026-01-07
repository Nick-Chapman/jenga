{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

module CommandLine
  ( Config(..),BuildMode(..),CacheDirSpec(..)
  , exec
  ) where

import Control.Monad (when)
import Locate (Dir,makeAbsoluteDir)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import Options.Applicative -- fully opened

data Config = Config
  { startDir :: Dir
  , homeDir :: Dir
  , cacheDir :: Dir
  , worker :: Bool
  , buildMode :: BuildMode
  , jnum :: Int
  , seePid :: Bool
  , cacheDirSpec :: CacheDirSpec
  , debugDemand :: Bool
  , debugExternal :: Bool
  , debugInternal :: Bool
  , debugLocking :: Bool
  , withPromotion :: Bool
  , reportRelative :: Bool
  , flagQ :: Bool
  , flagV :: Bool
  , flagA :: Bool
  }

data CacheDirSpec = CacheDirDefault | CacheDirChosen String | CacheDirTemp

data BuildMode
  = ModeBuild { targets :: [String] }
  | ModeTest
  | ModeRun { phony :: String }
  | ModeCat { target :: String }
  | ModeExec { exe :: String, args :: [String] }
  | ModeInstall { target :: String, destination ::  String }
  | ModeListPhony
  | ModeListTargets
  | ModeListRules

buildModeParser :: Parser BuildMode
buildModeParser = hsubparser $
  build <> test <> run <> cat <> exec <> install <> listT <> listR <> listP
  where
    build = command "build" $ info (ModeBuild <$> args) $
      progDesc "Bring the build up to date"
      where args = many <$> strArgument $ metavar "TARGETS" <> help "Targets to build"

    test = command "test" $ info (pure ModeTest) $
      progDesc "Test everything"

    run = command "run" $ info (ModeRun <$> phony) $
      progDesc "Build then run a phony-target"
      where phony = strArgument $ metavar "PHONY" <> help "Phony-target to run"

    cat = command "cat" $ info (ModeCat <$> target) $
      progDesc "Build then print a target"
      where target = strArgument $ metavar "TARGET" <> help "Target to print"

    exec = command "exec" $ info (ModeExec <$> exe <*> args) $
      progDesc "Build then execute a target with arguments"
      where
        exe = strArgument $ metavar "EXE" <> help "Executable target"
        args = many <$> strArgument $ metavar "ARGS" <> help "Arguments to pass"

    install = command "install" $ info (ModeInstall <$> target <*> dest) $
      progDesc "Build then install a target"
      where
        target = strArgument $ metavar "TARGET" <> help "Target to install"
        dest = strArgument $ metavar "DESTINATION" <> help "Destination to install as"

    listT = command "list-targets" $ info (pure ModeListTargets) $
      progDesc "List all build targets"

    listR = command "list-rules" $ info (pure ModeListRules) $
      progDesc "List all build rules"

    listP = command "list-phony" $ info (pure ModeListPhony) $
      progDesc "List all phony targets (can be run)"

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

      buildMode <- buildModeParser

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
        help "Suppress 'checked' and 'ran' messages"

      flagV <- switch $
        hidden <>
        short 'v' <>
        long "verbose" <>
        help "Extra messages while building"

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
      <> footer "For more information, see https://github.com/Nick-Chapman/jenga"
  let
    pic :: ParserInfo Config
    pic = info (helper <*> confParser) im

  customExecParser myPrefs pic
