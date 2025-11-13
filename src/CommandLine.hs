{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

module CommandLine
  ( LogMode(..),Config(..),BuildMode(..), CacheDirSpec(..)
  , exec
  ) where

import Options.Applicative

data LogMode = LogQuiet | LogNormal | LogActions

data Config = Config
  { buildMode :: BuildMode
  , args :: [FilePath]
  , worker :: Bool -- can't set on command line; but convenient to have in config
  , jnum :: Int
  , seePid :: Bool
  , cacheDirSpec :: CacheDirSpec
  , logMode :: LogMode
  , debugDemand :: Bool
  , debugExternal :: Bool
  , debugInternal :: Bool
  , debugLocking :: Bool
  , materializeCommaJenga :: Bool
  }

data CacheDirSpec = CacheDirDefault | CacheDirChosen String | CacheDirTemp

data BuildMode
  = ModeListTargets
  | ModeListRules
  | ModeBuild
  | ModeBuildAndRun FilePath [FilePath]

exec :: IO Config
exec = customExecParser
  (prefs (showHelpOnError <> showHelpOnEmpty))
  (info (subCommands <**> helper)
    ( fullDesc <> header "jenga: A build system" ))

subCommands :: Parser Config
subCommands =
  hsubparser
  (command "build"
    (info buildCommand
      (progDesc "Bring a build up to date")))
  <|>
  hsubparser
  (command "run"
    (info runCommand
      (progDesc "Build and run a single executable target")))

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
  sharedOptions LogNormal args buildMode

runCommand :: Parser Config
runCommand = do
  let
    buildMode = do
      exe <- strArgument (metavar "EXE" <> help "Target executable to build and run")
      exeArgs <- many (strArgument (metavar "EXE-ARG" <> help "Argument for target executable"))
      pure (ModeBuildAndRun exe exeArgs)
  let args = pure []
  sharedOptions LogQuiet args buildMode

sharedOptions :: LogMode -> Parser [FilePath] -> Parser BuildMode -> Parser Config
sharedOptions defaultLogMode args buildMode = do

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

  pure $ Config { .. }
