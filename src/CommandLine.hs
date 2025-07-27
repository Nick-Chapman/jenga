module CommandLine
  ( LogMode(..),Config(..),BuildMode(..), CacheDirSpec(..)
  , exec
  ) where

import Options.Applicative

data LogMode = LogQuiet | LogActions

data Config = Config
  { worker :: Bool -- can't set on command line; but convenient to have in config
  , jnum :: Int
  , seePid :: Bool
  , cacheDirSpec :: CacheDirSpec
  , keepSandBoxes :: Bool -- TODO: what is the point of this?
  , logMode :: LogMode
  , debugDemand :: Bool
  , debugExternal :: Bool
  , debugInternal :: Bool
  , debugLocking :: Bool
  , buildMode :: BuildMode
  , args :: [FilePath]
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
  (command "list-targets"
    (info listTargets
      (progDesc "List all targets")))
  <|>
  hsubparser
  (command "list-rules"
    (info listRules
      (progDesc "List all build rules")))
  <|>
  hsubparser
  (command "build"
    (info buildCommand
      (progDesc "Bring a build up to date")))
  <|>
  hsubparser
  (command "run"
    (info runCommand
      (progDesc "Build and run a single executable target")))


listTargets :: Parser Config
listTargets = sharedOptions LogQuiet
  <*> pure ModeListTargets
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

listRules :: Parser Config
listRules = sharedOptions LogQuiet
  <*> pure ModeListRules
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

buildCommand :: Parser Config
buildCommand = sharedOptions LogActions
  <*>
  (
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
  )
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

runCommand :: Parser Config
runCommand = sharedOptions LogQuiet
  <*> (
  ModeBuildAndRun <$>
    strArgument (metavar "EXE-TARGET"
                 <> help "target to build and execute")
    <*>
    many (strArgument (metavar "EXE-ARG"
                       <> help "arguent to pass to target executable"))
  )
  <*>
  pure []

-- TODO: try applicative do
sharedOptions :: LogMode -> Parser (BuildMode -> [FilePath] -> Config)
sharedOptions defaultLogMode = Config False
  <$>
  option auto (short 'j'
               <> value 1
               <> metavar "NUM_PROCS"
               <> help "Number of parallel jenga processes to run")
  <*> switch (short 'p'
              <> long "show-pid"
              <> help "Prefix log lines with pid")
  <*>
  (
    CacheDirChosen <$> strOption
    (short 'c' <> long "cache" <> metavar "DIR"
     <> help "Use .cache/jenga in DIR instead of $HOME"
    )
    <|>
    flag' CacheDirTemp
    (short 'f' <> long "temporary-cache"
     <> help "Build using temporary cache to force build actions")
    <|> pure CacheDirDefault
  )
  <*>
  switch (short 'k' <> long "keep-sandboxes"
          <> help "Keep sandboxes when build completes")
  <*>
  (
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
  )
  <*> switch (long "debug-demand" <> help "Debug demanded build targets")
  <*> switch (long "debug-external" <> help "Debug calls to md5sum")
  <*> switch (long "debug-internal" <> help "Debug file system access")
  <*> switch (long "debug-locking" <> help "Debug locking behaviour")
