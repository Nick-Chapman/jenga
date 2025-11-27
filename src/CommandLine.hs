{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

module CommandLine
  ( LogMode(..),Config(..),BuildMode(..), CacheDirSpec(..)
  , exec
  ) where

import Options.Applicative
import System.Directory (getCurrentDirectory)

import Locate (Dir,makeDir)

data LogMode = LogQuiet | LogNormal | LogActions

data Config = Config
  { startDir :: Dir
  , worker :: Bool -- can't set on command line; but convenient to have in config
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
  , materializeCommaJenga :: Bool
  , withPromotion :: Bool
  , strict :: Bool
  , reportRelative :: Bool
  }

data CacheDirSpec = CacheDirDefault | CacheDirChosen String | CacheDirTemp

data BuildMode
  = ModeListTargets
  | ModeListRules
  | ModeBuild
  | ModeExec FilePath [FilePath]
  | ModeInstall FilePath FilePath
  | ModeRun [String]

exec :: IO Config
exec = do
  startDir <- makeDir "[CommandLine.exec]" <$> getCurrentDirectory
  execAt startDir

execAt :: Dir -> IO Config
execAt startDir = do
  customExecParser
    -- TODO: fix digest-id shown in usage message when run by "jenga exec src/jenga.exe"
    (prefs (showHelpOnError <> showHelpOnEmpty))
    (info (subCommands <**> helper)
     ( fullDesc <> header "jenga: A build system" ))

  where
  subCommands :: Parser Config
  subCommands =
    -- TODO: fix "COMMAND" shown in usage message
    hsubparser (command "build" (info buildCommand (progDesc "Bring a build up to date"))) <|>
    hsubparser (command "exec" (info execCommand (progDesc "build; then run a single executable target"))) <|>
    hsubparser (command "install" (info installCommand (progDesc "build; then install a single executable"))) <|>
    hsubparser (command "run" (info runCommand (progDesc "build; then run a list of actions"))) <|>
    hsubparser (command "test" (info testCommand (progDesc "build; then run the 'test' action")))

  buildCommand :: Parser Config
  buildCommand = do
    let
      buildMode =
        flag' ModeListTargets
        (short 't'
          <> long "list-targets"
          <> help "List buildable targets")
        <|>
        flag' ModeListRules
        (short 'r'
          <> long "list-rules"
          <> help "List generated rules")
        <|>
        pure ModeBuild
    let
      args =
        many (strArgument (metavar "DIRS" <> help "Directories to search for build rules; default CWD"))
    sharedOptions startDir LogNormal args buildMode

  execCommand :: Parser Config
  execCommand = do
    let
      buildMode = do
        exe <- strArgument (metavar "EXE" <> help "Target executable to build and run")
        exeArgs <- many (strArgument (metavar "ARG+" <> help "Arguments for target executable"))
        pure (ModeExec exe exeArgs)
    let args = pure []
    sharedOptions startDir LogQuiet args buildMode

  installCommand :: Parser Config
  installCommand = do
    let
      buildMode = do
        exe <- strArgument (metavar "EXE" <> help "Executable to build and install")
        dest <- strArgument (metavar "DEST" <> help "Destination of installed executable")
        pure (ModeInstall exe dest)
    let args = pure []
    sharedOptions startDir LogQuiet args buildMode

  runCommand :: Parser Config
  runCommand = do
    let
      buildMode = do
        ps <- some (strArgument (metavar "PHONY+" <> help "Phony targets to build and run"))
        pure (ModeRun ps)
    let args = pure []
    sharedOptions startDir LogNormal args buildMode

  testCommand :: Parser Config
  testCommand = do
    let args = pure []
    sharedOptions startDir LogNormal args (pure (ModeRun ["test"]))

sharedOptions :: Dir -> LogMode -> Parser [FilePath] -> Parser BuildMode -> Parser Config
sharedOptions startDir defaultLogMode args buildMode = do

  args <- args
  buildMode <- buildMode

  let worker = False

  jnum <-
    option auto (short 'j'
                 <> value 1
                  <> metavar "NUM_PROCS"
                  <> help "Number of parallel jenga processes to run")

  seePid <-
    switch (short 'p'
             <> long "show-pid"
             <> help "Prefix log lines with pid")

  cacheDirSpec <-
    CacheDirChosen <$> strOption
    (short 'c' <> long "cache" <> metavar "DIR"
     <> help "Use .cache/jenga in DIR instead of $HOME"
    )
    <|>
    flag' CacheDirTemp
    (short 'f' <> long "temporary-cache"
     <> help "Build using temporary cache to force build actions")
    <|> pure CacheDirDefault

  logMode <-
    flag' LogActions
    (short 'a'
     <> long "show-actions"
      <> help "Build showing actions run")
    <|>
    flag' LogQuiet
    (short 'q'
     <> long "quiet"
     <> help "Build quietly, except for errors")
    <|>
    pure defaultLogMode

  debugDemand <- switch (long "debug-demand" <> help "Debug demanded build targets")
  debugExternal <- switch (long "debug-external" <> help "Debug calls to md5sum")
  debugInternal <- switch (long "debug-internal" <> help "Debug file system access")
  debugLocking <- switch (long "debug-locking" <> help "Debug locking behaviour")

  materializeCommaJenga <-
    switch (short 'm' <> long "materialize"
            <> help "Materialize all build artifacts in ,jenga directory")

  withPromotion <-
    switch (long "promote"
            <> help "Promote test output to .expected files")

  strict <- not <$>
    switch (long "sloppy" -- TODO kill
            <> help "Run jenga actions with a sloppy environment")

  reportRelative <-
    switch (long "rel"
            <> help "Report paths relative to starting directory")

  pure $ Config { .. }
