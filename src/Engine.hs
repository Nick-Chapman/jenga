
module Engine (main) where

import Control.Exception (try,SomeException)
import Control.Monad (ap,liftM)
import Control.Monad (when,forM)
import Data.Hash.MD5 qualified as MD5
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (stripPrefix,intercalate)
import Data.List qualified as List (foldl')
import Data.List.Ordered (nubSort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile)
import System.Environment (lookupEnv)
import System.Exit(ExitCode(..))
import System.FileLock (FileLock,tryLockFile,SharedExclusive(Exclusive),unlockFile)
import System.IO (hFlush,hPutStrLn,withFile,IOMode(WriteMode),hPutStr)
import System.IO qualified as IO (stderr,stdout)
import System.Posix.Files (fileExist,createLink,removeLink,getFileStatus,getSymbolicLinkStatus,fileMode,intersectFileModes,setFileMode,getFileStatus,isDirectory,isSymbolicLink)
import System.Posix.Process (forkProcess)
import System.Process (ProcessHandle,waitForProcess,CreateProcess(env),shell,proc,createProcess,readCreateProcess,readCreateProcessWithExitCode,getCurrentPid)
import Text.Printf (printf)
import Text.Read (readMaybe)

import CommandLine (LogMode(..),Config(..),BuildMode(..),CacheDirSpec(..))
import CommandLine qualified (exec)
import Interface (G(..),D(..),Rule(..),Action(..),Target(..),Artifact(..), Key(..),What(..))
import Locate (Loc,pathOfLoc,Dir,makeAbsoluteDir,pathOfDir,takeDir,(</>),insistLocIsDir,Tag,makeTag,stringOfTag,takeBase,locOfDir)
import Syntax qualified (elaborate)
import WaitPid (waitpid)

main :: IO ()
main = do
  config <- CommandLine.exec
  engineMain config

----------------------------------------------------------------------
-- Engine main

engineMain :: Config -> IO ()
engineMain config@Config{startDir,homeDir,cacheDirSpec,logMode} = do

  let userProg = mkUserProg config
  let quiet = case logMode of LogQuiet -> True; _ -> False
  myPid <- getCurrentPid

  cacheDir :: Dir <- insistLocIsDir <$> -- TODO: move into CommandLine
    case cacheDirSpec of
      CacheDirDefault -> do
        lookupEnv "XDG_CACHE_HOME" >>= \case
          Just cache -> do
            pure (makeAbsoluteDir cache </> "jenga")
          Nothing -> do
            pure (homeDir </> ".cache/jenga")
      CacheDirChosen dirString -> do
        pure (insistLocIsDir (startDir </> dirString) </> ".cache/jenga")
      CacheDirTemp -> do
        let loc = locOfDir (makeAbsoluteDir (printf "/tmp/%s/.cache/jenga" (show myPid)))
        when (not quiet) $ putOut (printf "using temporary cache: %s" (pathOfLoc loc))
        pure loc

  config <- pure $ config { cacheDir } -- override the initial undefined value for cacheDir

  elaborateAndBuild config userProg

dropPrefixChecked :: String -> String -> String
dropPrefixChecked prefix str =
  if prefix == str then "." else
    case stripPrefix (prefix++"/") str of
      Just suffix -> suffix
      Nothing -> str

relppKey :: Dir -> Key -> String
relppKey startDir (Key loc) =
  dropPrefixChecked (pathOfDir startDir) (pathOfLoc loc)

ppKey :: Config -> Key -> String
ppKey Config{reportRelative,startDir} (Key loc) =
  case reportRelative of
    True -> relppKey startDir (Key loc)
    False -> pathOfLoc loc

ppKeys :: Config -> [Key] -> String
ppKeys config = unwords . map (ppKey config)

elaborateAndBuild :: Config -> UserProg -> IO ()
elaborateAndBuild config@Config{logMode,startDir,buildMode,args} userProg = do
  case buildMode of

    ModeListTargets -> do
      _fbs :: FBS <- runBuild config $ \config@Config{worker} -> do
         system <- runElaboration config (userProg args)
         let System{rules} = system
         when (not worker) $ do
           sequence_
             [ BLog t
             | Rule{target} <- rules
             , t <-
                 case target of
                   Artifacts arts -> [ ppKey config key | Artifact{key} <- arts ]
                   Phony _name -> []
             ]
      pure ()

    ModeListRules -> do
      _fbs :: FBS <- runBuild config $ \config@Config{worker} -> do
        system <- runElaboration config (userProg args)
        let System{how,rules} = system
        when (not worker) $ do
          staticRules :: [StaticRule] <- concat <$>
            sequence [ do (deps,action) <- gatherDeps emptyChain config how depcom
                          pure [ StaticRule { dir, target, deps, action } ]
                     | Rule{dir,target,depcom} <- rules
                     ]
          BLog (intercalate "\n\n" (map (ppStaticRule config) staticRules))
      pure ()

    ModeCat src0 -> do
      let src = startDir </> src0
      fbs :: FBS <- runBuild config $ \config -> do
        system <- runElaboration config (userProg ["."])
        let System{how} = system
        _digest <- buildArtifact emptyChain config how (Key src)
        pure ()
      newReport config fbs
      runX config $ do
        case lookFBS fbs (Key src) of
          Nothing -> pure () -- we can't cat what didn't get built
          Just digest -> do
            let cacheFile = cacheFileLoc config digest
            contents <- XReadFile cacheFile
            XIO $ putStr contents

    ModeExec exe0 argsForExe -> do
      let exe = startDir </> exe0
      fbs :: FBS <- runBuild config $ \config -> do
        system <- runElaboration config (userProg ["."])
        let System{how} = system
        _digest <- buildArtifact emptyChain config how (Key exe)
        pure ()
      newReport config fbs
      runX config $ do
        case lookFBS fbs (Key exe) of
          Nothing -> pure () -- we can't exec what didn't get built
          Just digest -> do
            let cacheFile = cacheFileLoc config digest
            XIO $ do
              -- TODO: could/should we actually do an exec here? (as in fork/exec)
              (_,_,_,h :: ProcessHandle) <- createProcess (proc (pathOfLoc cacheFile) argsForExe)
              exitCode <- waitForProcess h
              putStr (seeFailureExit exitCode) -- TODO: propagate exit-code instead of print

    ModeInstall src0 dest0 -> do
      let src = startDir </> src0
      fbs :: FBS <- runBuild config $ \config -> do
        system <- runElaboration config (userProg ["."])
        let System{how} = system
        _digest <- buildArtifact emptyChain config how (Key src)
        pure ()
      newReport config fbs
      runX config $ do
        case lookFBS fbs (Key src) of
          Nothing -> pure () -- we can't install what didn't get built
          Just digest -> do
            let dest = startDir </> dest0
            installDigest config digest dest
            XIO $ do
              let quiet = case logMode of LogQuiet -> True; _ -> False
              when (not quiet) $ do
                printf "installed %s\n" (ppKey config (Key dest))

    ModeRun names -> do -- including "test" == "run test"
      fbs :: FBS <- runBuild config $ \config -> do
        system <- runElaboration config (userProg args)
        let System{how} = system
        parallel_ [ buildPhony config how name | name <- names ]
      newReport config fbs
      pure ()

    ModeBuild -> do
      fbs :: FBS <- runBuild config $ \config -> do
        system <- runElaboration config (userProg args)
        buildEverythingInSystem config system
      newReport config fbs
      pure ()  -- TODO: exit code with #errors

newReport :: Config -> FBS -> IO ()
newReport Config{logMode,worker} FBS{countRules=nr,failures} = do
  let quiet = case logMode of LogQuiet -> True; _ -> False
  when (not worker && length failures > 0) $ do
    printf "Build failed for %s:\n%s\n" (pluralize (length failures) "reason")
      (intercalate "\n" [ printf "(%d) %s" i r | (i,r) <- zip [1::Int ..] failures ])
  when (not quiet && not worker) $ do
    when (length failures == 0) $ do
      -- We flush this report to be sure is proceeds whatever follows the build; i.e. cat, exec
      putOut $ printf "checked %s" (pluralize nr "rule")

pluralize :: Int -> String -> String
pluralize n what = printf "%d %s%s" n what (if n == 1 then "" else "s")

runBuild :: Config -> (Config -> B ()) -> IO FBS
runBuild config f = do
  nCopies config $ \config -> do
    runX config $ do
      runB config (f config)

nCopies :: Config -> (Config -> IO a) -> IO a
nCopies config@Config{jnum} f =
  if jnum < 1 then error "nCopies<1" else do
    children <- sequence $ replicate (jnum-1) $ do
      forkProcess $ do
        _childRes <- f $ config { worker = True }
        pure ()
    parentRes <- f config
    mapM_ waitpid children
    pure parentRes

buildEverythingInSystem :: Config -> System -> B ()
buildEverythingInSystem config system = do
  let System{rules,how} = system
  let
    allArtifacts =
        [ art
        | Rule{target} <- rules
        , let arts = case target of Artifacts arts -> arts ; Phony{} -> []
        , art <- arts
        ]
  parallel_
    [ do _ <- buildArtifact emptyChain config how key; pure ()
    | Artifact { key } <- allArtifacts
    ]

data StaticRule = StaticRule
  { dir :: Dir
  , target :: Target
  , deps :: [Key]
  , action :: Action
  }

ppStaticRule :: Config -> StaticRule -> String
ppStaticRule config StaticRule{target,deps,action=Action{commands}} = do
  let sep = printf "\n  "
  printf "%s : %s%s%s" (ppTarget config target) (ppKeys config deps) sep (intercalate sep commands)

ppTarget :: Config -> Target -> String
ppTarget config = \case
  Artifacts arts -> ppKeys config [ key | Artifact { key } <- arts ]
  Phony phonyName -> "*" ++ phonyName

installDigest :: Config -> Digest -> Loc -> X ()
installDigest config digest destination = do
  let cacheFile = cacheFileLoc config digest
  do
    needToLink <-
      XFileExists destination >>= \case
        False -> pure True
        True -> do
          oldDigest <- XDigest destination
          pure $ (digest /= oldDigest)
    when needToLink $ do
      XFileExists destination >>= \case
        True -> XTransferFileMode destination cacheFile
        False -> pure ()
      XUnLink destination
      XMakeDir (takeDir destination)
      -- We must copy (rather than hard-link) the cacheFile to the destination.
      -- Or else edits to the destination file will corrupt out cache file.
      XCopyFile cacheFile destination

----------------------------------------------------------------------
-- locations for cache, sandbox etc

cacheFileLoc :: Config -> Digest -> Loc
cacheFileLoc config (Digest str) = cacheFilesDir config </> str

cacheFilesDir :: Config -> Dir
cacheFilesDir Config{cacheDir} = insistLocIsDir (cacheDir </> "files")

tracesDir :: Config -> Dir
tracesDir Config{cacheDir} = insistLocIsDir (cacheDir </> "traces")

----------------------------------------------------------------------
-- mkUserProg: not really a sensible name anymore

type UserProg = [String] -> G ()

mkUserProg :: Config -> UserProg
mkUserProg = \config args -> do
  let Config{startDir} = config
  dotJengas <- findDotJengas startDir args
  sequence_ [ Syntax.elaborate (ppKey config) config (Key dotJenga) | dotJenga <- dotJengas ]

findDotJengas :: Dir -> [String] -> G [Loc] -- TODO: take [Dir]
findDotJengas dir args = do
  let args' = case args of [] -> ["."]; _ -> args
  dotJengas <- concat <$> sequence [ findFrom (dir </> arg) | arg <- args' ]
  pure (reverse $ nubSort dotJengas) -- reverse so subdirs come earlier

findFrom :: Loc -> G [Loc]
findFrom loc = do
  GWhat loc >>= \case
    Missing -> pure []
    Link -> pure []
    File -> pure []
    Directory{entries} -> do
      case "build.jenga" `elem` entries of
        False -> dotJengas
        True -> do
          let dotJenga = dir </> "build.jenga"
          (dotJenga:) <$> dotJengas
      where
        dir = insistLocIsDir loc
        dotJengas = concat <$> sequence [ findFrom (dir </> e)
                                      | e <- entries , not (blockName e)]

blockName :: String -> Bool
blockName = \case
  ".git" -> True
  ".stack-work" -> True
  ".cache" -> True
  "_build" -> True -- dune
  _ -> False

----------------------------------------------------------------------
-- Elaborate

data System = System { rules :: [Rule], how :: How }

data How = How { ahow :: Map Key Rule -- map from (artifact)-key to rule
               , phow :: Map String [Rule] -- map from phoy name to set of rules
               }

runElaboration :: Config -> G () -> B System
runElaboration config m =
  loop m system0 k0 >>= \case
    system@System{rules} -> do
      pure system { rules = reverse rules }
  where
    system0 :: System
    system0 = System { rules = [], how = How { ahow = Map.empty, phow = Map.empty } }

    k0 :: System -> () -> B System
    k0 s () = pure s

    loop :: G a -> System -> (System -> a -> B System) -> B System
    loop m system k = case m of
      GRet a -> k system a
      GBind m f -> loop m system $ \system a -> loop (f a) system k
      GLog mes -> do
        Execute (XLog (printf "log: %s" mes))
        k system ()
      GFail mes -> bfail mes --ignore k
      GRule rule -> do
        let Rule{target} = rule
        case target of
          Phony phonyName -> do
            let System{rules,how} = system
            let How{phow} = how
            let existingRulesForThisPhony = maybe [] id $ Map.lookup phonyName phow
            phow <- pure $ Map.insert phonyName (rule : existingRulesForThisPhony) phow
            k system { rules = rule : rules, how = how { phow } } ()

          Artifacts arts -> do
            xs <- sequence [ do b <- Execute (XFileExists loc); pure (key,b)
                           | Artifact { materialize = False, key = key@(Key loc) } <- arts
                           ]
            -- we only report a clash for non-materializing targets
            case [ key | (key,isSource) <- xs, isSource ] of
              clashS@(_:_) -> do
                bfail $ printf "rule targets clash with source :%s" (ppKeys config clashS)
              [] -> do
                let targetKeys = [ key | Artifact { key } <- arts ]
                let System{rules,how} = system
                let How{ahow} = how
                case filter (\k -> Map.member k ahow) targetKeys of
                  clashR@(_:_) -> do
                    bfail $ printf "rule targets defined by earlier rule :%s" (ppKeys config clashR)
                  [] -> do
                    ahow <- pure $ List.foldl' (flip (\k -> Map.insert k rule)) ahow targetKeys
                    k system { rules = rule : rules, how = how { ahow } } ()

      GWhat loc -> do
        Execute (xwhat loc) >>= k system

      GReadKey key -> do
        let System{how} = system
        contents <- readKey emptyChain config how key -- make cause building
        k system contents

      GExistsKey key -> do
        let System{how} = system
        b <- existsKey how key
        k system b

      GPar g1 g2 -> do -- TODO: actually work in parallel
        loop g1 system $ \system a1 -> do
          loop g2 system $ \system a2 -> do
            k system (a1,a2)

xwhat :: Loc -> X What
xwhat loc = do
  XFileExists loc >>= \case
    False -> pure Missing
    True -> do
      XIsSymbolicLink loc >>= \case
        True -> pure Link
        False -> do
          XIsDirectory loc >>= \case
            False -> pure File
            True -> Directory <$> XListDir loc

tagOfKey :: Key -> Tag
tagOfKey (Key loc) = takeBase loc

----------------------------------------------------------------------
-- Build

buildPhony :: Config -> How -> String -> B ()
buildPhony config how@How{phow} phonyName = do
  case maybe [] id $ Map.lookup phonyName phow of
    [] ->
      bfail (printf "no rules for phony target '%s'" phonyName)
    rules ->
      parallel_ [ do _ :: WitMap <- buildRule emptyChain config how rule; pure ()
                | rule <- rules
                ]

buildArtifact :: Chain -> Config -> How -> Key -> B Digest
buildArtifact chain config@Config{worker,debugDemand} how@How{ahow} = do
  -- TODO: document this flow.
  BMemoKey chain $ \sought -> do
    chain <- pure $ pushChain sought chain -- this is where we extend the dependency chain
    case Map.lookup sought ahow of
      Nothing -> do
        let Key loc = sought
        Execute (XFileExists loc) >>= \case
          False -> do
            bfail (printf "'%s' : is not source and has no build rule" (ppKey config sought))
          True -> do
            digest <- copyIntoCache config loc
            pure digest

      Just rule -> do
        when (not worker && debugDemand) $ BLog (printf "B: Require: %s" (ppKey config sought))
        -- We only memoize the run of rule targeting artifacts (not phonys)
        wtargets <- BMemoRule (buildRuleMat chain config how) rule
        let digest = lookWitMap (tagOfKey sought) wtargets
        pure digest

-- build a rule; then do user materialization ("!") if specified
buildRuleMat :: Chain -> Config -> How -> Rule -> B WitMap
buildRuleMat chain config how rule = do
  wtargets <- buildRule chain config how rule
  let Rule{target} = rule
  case target of
    Phony{} -> pure ()
    Artifacts arts -> do
      sequence_ [ do Execute $ installDigest config digest loc
                | Artifact {materialize,key = key@(Key loc)} <- arts
                , materialize
                , let digest = lookWitMap (tagOfKey key) wtargets
                ]
  pure wtargets

buildRule :: Chain -> Config -> How -> Rule -> B WitMap
buildRule chain config@Config{debugLocking} how rule = do
  let Rule{dir,target,depcom} = rule
  let arts = case target of Artifacts arts -> arts ; Phony{} -> []
  let targets = [ key | Artifact { key } <- arts ]
  (deps,action@Action{commands}) <- gatherDeps chain config how depcom
  let sr = StaticRule { dir, target, deps, action } -- used for message context
  wdeps <- (WitMap . Map.fromList) <$>
    parallel [ do digest <- buildArtifact chain config how dep; pure (tagOfKey dep,digest)
             | dep <- deps
             ]
  let witKey = WitnessKey { targets = map tagOfKey targets, commands, wdeps }
  let wkd = digestWitnessKey witKey
  -- If the witness trace already exists, use it.
  when debugLocking $ Execute $ XLog (printf "L: looking for witness: %s" (show wkd))
  verifyWitness config sr wkd >>= \case
    Just w -> pure w
    Nothing -> do
      -- If not, someone has to run the job...
      when debugLocking $ Execute $ XLog (printf "L: no witness; job must be run: %s" (show wkd))
      tryGainingLock config debugLocking wkd $ \case
        True -> do
          -- We got the lock, check again for the trace...
          when debugLocking $ Execute $ XLog (printf "L: look again for witness: %s" (show wkd))
          verifyWitness config sr wkd >>= \case
            Just w -> pure w
            Nothing -> do
              when debugLocking $ Execute $ XLog (printf "L: still no witness; running jobs NOW: %s" (show wkd))
              -- We have the lock and there is still no trace, so we run the job....
              runActionSaveWitness config sr action wkd wdeps rule
        False -> do
          when debugLocking $ Execute $ XLog (printf "L: running elsewhere: %s" (show wkd))
          -- Another process beat us to the lock; it's running the job.
          awaitLockYielding config wkd
          -- Now it will have finished.
          verifyWitness config sr wkd >>= \case
            Just w -> pure w
            Nothing ->
              bfail (printf "failed to verify witness for rule targeting: %s" (ppTarget config target))

verifyWitness :: Config -> StaticRule -> WitKeyDigest -> B (Maybe WitMap)
verifyWitness config sr wkd = do
  lookupWitness config wkd >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      case wit of
        WitnessFAIL ares -> do
          Execute (showActionRes config sr ares)
          actionFailed config sr
        WitnessSUCC{wtargets,ares} -> do
          let WitMap m = wtargets
          ok <- all id <$> sequence [ existsCacheFile config digest | (_,digest) <- Map.toList m ]
          if not ok then pure Nothing else do
            Execute (showActionRes config sr ares)
            pure (Just wtargets)

awaitLockYielding :: Config -> WitKeyDigest -> B ()
awaitLockYielding config@Config{debugLocking} wkd = loop 0
  where
    loop :: Int -> B ()
    loop i = do
      BYield
      tryGainingLock config False wkd $ \case
        False -> loop (i+1)
        True -> do
          when debugLocking $ Execute $ XLog (printf "L: spun (%d) awaiting lock %s" i (show wkd))
          pure ()

tryGainingLock :: Config -> Bool -> WitKeyDigest -> (Bool -> B a) -> B a
tryGainingLock config debug wkd f = do
  let lockPath = tracesDir config </> (show wkd ++ ".lock")
  -- Try to gain the lock...
  Execute (XIO (tryLockFile (pathOfLoc lockPath) Exclusive)) >>= \case
    Just (lock::FileLock) -> do
      when debug $ Execute $ XLog (printf "L: locked: %s" (show wkd))
      -- Run the critical section while the lock is held.
      res <- BCatch (f True)
      -- Critical section finished; release the lock (unlink and unlok).
      Execute $ do
        XUnLink lockPath
        XIO (unlockFile lock)
        when debug $ XLog $ printf "L: unlocked %s" (show wkd)
      BResult res
    Nothing ->
      -- We lost the race; another process has the lock.
      f False


gatherDeps :: Chain -> Config -> How -> D a -> B ([Key],a)
gatherDeps chain config how d = loop d [] k0
  where
    k0 xs a = pure (reverse xs,a)
    loop :: D a -> [Key] -> ([Key] -> a -> B ([Key],b)) -> B ([Key], b)
    loop d xs k = case d of
      DRet a -> k xs a
      DBind m f -> loop m xs $ \xs a -> loop (f a) xs k
      DLog mes -> do
        Execute (XLog (printf "log: %s" mes))
        k xs ()
      DNeed key -> k (key:xs) ()
      DReadKey key -> do
        contents <- readKey chain config how key
        k xs contents
      DExistsKey key -> do
        b <- existsKey how key
        k xs b

readKey :: Chain -> Config -> How -> Key -> B String
readKey chain config how key = do
  digest <- buildArtifact chain config how key
  let cacheFile = cacheFileLoc config digest
  Execute (XReadFile cacheFile)

existsKey :: How -> Key -> B Bool
existsKey How{ahow} key =
  if Map.member key ahow
  then pure True
  else do
    let Key loc = key
    Execute (XFileExists loc)

runActionSaveWitness :: Config -> StaticRule -> Action -> WitKeyDigest -> WitMap -> Rule -> B WitMap
runActionSaveWitness config sr action wkd depWit rule = do
  sandbox <- BNewSandbox
  Execute (XMakeDir sandbox)
  setupInputs config sandbox depWit
  ares <- BRunActionInDir sandbox action
  Execute (showActionRes config sr ares)
  let ok = actionResIsOk ares
  case ok of
    False -> do
      let wit = WitnessFAIL ares
      saveWitness config wkd wit
      actionFailed config sr
    True -> do
      wtargets <- cacheOutputs config sandbox rule -- If this fails we dont write a witness. TODO: is that right?
      let wit = WitnessSUCC { wtargets, ares }
      saveWitness config wkd wit
      pure wtargets

actionFailed :: Config -> StaticRule -> B a
actionFailed config StaticRule{target} = do
  bfail (printf "action failed for rule targeting: %s" (ppTarget config target))


setupInputs :: Config -> Dir -> WitMap -> B ()
setupInputs config sandbox (WitMap m1) = do
  sequence_
    [ do
        let src = cacheFileLoc config digest
        let dest = sandbox </> stringOfTag tag
        Execute $ hardLink src dest
    | (tag,digest) <- Map.toList m1
    ]

hardLink :: Loc -> Loc -> X ()
hardLink src dest = do
  XTryHardLink src dest >>= \case
    Nothing -> pure ()
    Just e -> error e

cacheOutputs :: Config -> Dir -> Rule -> B WitMap
cacheOutputs config sandbox Rule{target} = do
  let arts = case target of Artifacts arts -> arts ; Phony{} -> []
  let targets = [ key | Artifact { key } <- arts ]
  WitMap . Map.fromList <$> sequence
    [ do
        let tag = tagOfKey target
        let sandboxLoc = sandbox </> stringOfTag tag
        Execute (XFileExists sandboxLoc) >>= \case
          False -> do
            bfail (printf "'%s' : not produced as declared by rule" (ppKey config target))
          True -> do
            digest <- linkIntoCache config sandboxLoc
            pure (tag,digest)
    | target <- targets
    ]

copyIntoCache :: Config -> Loc -> B Digest
copyIntoCache config loc = do
  digest <- Execute (XDigest loc)
  let file = cacheFileLoc config digest
  Execute (XFileExists file) >>= \case
    True -> pure ()
    False -> do
      Execute $ do
        XCopyFile loc file
        XMakeReadOnly file
        pure ()

  pure digest

linkIntoCache :: Config -> Loc -> B Digest
linkIntoCache config loc = do
  digest <- Execute (XDigest loc)
  let file = cacheFileLoc config digest
  Execute $ do
    XFileExists file >>= \case
      True -> XTransferFileMode loc file
      False -> do
        XTryHardLink loc file >>= \case
          Nothing -> pure ()
          Just err -> do
            -- job locking ensures this can't happen
            error (printf "linkIntoCache/HardLink: failure: loc=%s, file=%s, err=%s"
                   (pathOfLoc loc) (pathOfLoc file) (show err)
                  )
    XMakeReadOnly file
  pure digest

-- message digest of a file; computed by call to external md5sum
data Digest = Digest String deriving (Eq)

instance Show Digest where show (Digest str) = str

----------------------------------------------------------------------
-- Build witnesses (AKA constructive traces)

type Witness = WitnessValue

data WitnessValue
  = WitnessSUCC { wtargets :: WitMap, ares :: ActionRes }
  | WitnessFAIL ActionRes

data WitnessKey = WitnessKey
  { targets :: [Tag]
  , commands :: [String]
  , wdeps :: WitMap
  } deriving Show

-- TODO: Filemode should be included in the target WitMap to solve the problem of a forgotten chmod +x on a generated script. Currently the callers wont be triggered to run again because they see no change in their deps; so they will be stuck showing the error which occurred when the script did not have exec perm.
data WitMap = WitMap (Map Tag Digest) deriving Show

-- message digest of a witness key; computer by internal MD5 code
data WitKeyDigest = WitKeyDigest String

instance Show WitKeyDigest where show (WitKeyDigest str) = str

lookWitMap :: Tag -> WitMap -> Digest
lookWitMap tag (WitMap m) = maybe err id $ Map.lookup tag m
  where err = error (show ("lookWitMap",tag,Map.keys m))

digestWitnessKey :: WitnessKey -> WitKeyDigest
digestWitnessKey wk = WitKeyDigest (MD5.md5s (MD5.Str (show wk)))

lookupWitness :: Config -> WitKeyDigest -> B (Maybe Witness)
lookupWitness config wkd = do
  let witFile = witnessFile config wkd
  Execute (XFileExists witFile) >>= \case
    False -> pure Nothing
    True -> Execute $ do
      contents <- XReadFile witFile
      case importWitness contents of
        Just wit -> pure (Just wit)
        Nothing -> do
          -- We failed to parse rhe witness file. Perhaps it got corrupted.
          -- Or perhaps we encountered a witness with an out-of-date format.
          -- XLogErr (printf "removing bad witness file: %s" (show witFile))
          XUnLink witFile
          pure Nothing

existsCacheFile :: Config -> Digest -> B Bool
existsCacheFile config digest = do
  let file = cacheFileLoc config digest
  Execute (XFileExists file)

saveWitness :: Config -> WitKeyDigest -> Witness -> B ()
saveWitness config wkd wit = do
  let witFile = witnessFile config wkd
  Execute $ do
    XWriteFile (exportWitness wit ++ "\n") witFile

witnessFile :: Config -> WitKeyDigest -> Loc
witnessFile config wkd = tracesDir config </> show wkd

----------------------------------------------------------------------
-- export Witnesses via flatter "Q" types (Q is a meaningless prefix)

importWitness :: String -> Maybe Witness
importWitness s = fromQ <$> readMaybe s

exportWitness :: Witness -> String
exportWitness = show . toQ

data QWitness = TRACE [QCommandRes] (Maybe QWitMap) -- Nothing indicates failure
  deriving (Show,Read)

data QCommandRes = RUN
  { exitCode :: ExitCode
  , stdout :: String
  , stderr :: String
  }
  deriving (Show,Read)

type QWitMap = [(FilePath,QDigest)] -- note: just the basename of every path
type QDigest = String

toQ :: Witness -> QWitness
toQ = \case
  WitnessFAIL (ActionRes xs) -> do
    let cs = [ RUN{exitCode,stdout,stderr} | CommandRes{exitCode,stdout,stderr} <- xs ]
    TRACE cs Nothing
  WitnessSUCC{wtargets=WitMap m,ares=ActionRes xs} -> do
    let cs = [ RUN{exitCode,stdout,stderr} | CommandRes{exitCode,stdout,stderr} <- xs ]
    let targets = [ (stringOfTag tag,digest) | (tag,Digest digest) <- Map.toList m ]
    TRACE cs (Just targets)

fromQ :: QWitness -> Witness
fromQ = \case
  TRACE cs topt -> do
    let ares = ActionRes [ CommandRes{exitCode,stdout,stderr} | RUN{exitCode,stdout,stderr} <- cs ]
    case topt of
      Nothing -> WitnessFAIL ares
      Just targets -> do
        let wtargets = WitMap (Map.fromList [ (makeTag fp,Digest digest) | (fp,digest) <- targets ])
        WitnessSUCC{wtargets,ares}

----------------------------------------------------------------------
-- Result from running actions

data ActionRes = ActionRes [CommandRes]

data CommandRes = CommandRes
  { exitCode :: ExitCode
  , stdout :: String
  , stderr :: String
  }

actionResIsOk :: ActionRes -> Bool
actionResIsOk (ActionRes xs) =
  all id [ ok
         | CommandRes{exitCode} <- xs
         , let ok = case exitCode of ExitSuccess -> True; ExitFailure{} -> False
         ]

showActionRes :: Config -> StaticRule -> ActionRes -> X ()
showActionRes config@Config{worker} sr ar = do
  when (not worker && anythingToSee ar) $ do
    XIO $ putStr (seeActionResAndContext config sr ar)

anythingToSee :: ActionRes -> Bool
anythingToSee (ActionRes xs) =
  any id [ isFailure exitCode || stdout/="" || stderr/=""
         | CommandRes {exitCode,stdout,stderr} <- xs ]
  where
    isFailure = \case ExitFailure{} -> True; ExitSuccess -> False

seeActionResAndContext :: Config -> StaticRule -> ActionRes -> String
seeActionResAndContext config sr@StaticRule{action=Action{commands}} (ActionRes xs) =
  if length commands /= length xs then error "seeActionResAndContext" else
    seeStaticRule config sr ++
    concat [ seeCommandAndRes command res | (command,res) <- zip commands xs ]

seeStaticRule :: Config -> StaticRule -> String
seeStaticRule config StaticRule{dir,target,deps} = do
  seeDir dir ++ "\n" ++ seeRule target deps ++ "\n"
  where
    seeDir :: Dir -> String
    seeDir dir = "(directory) " ++ ppKey config (Key (locOfDir dir))

    seeRule :: Target -> [Key] -> String
    seeRule target deps = "(rule) " ++ seeTarget target ++ " : " ++ seeKeys deps

    seeTarget :: Target -> String
    seeTarget = \case
      Artifacts arts -> seeKeys [ key | Artifact { key } <- arts ]
      Phony phonyName -> "*" ++ phonyName

    seeKeys :: [Key] -> String
    seeKeys = unwords . map seeKey

    seeKey :: Key -> String
    seeKey key = ppKey config key

seeCommandAndRes :: String -> CommandRes -> String
seeCommandAndRes command CommandRes{exitCode,stdout,stderr} =
  "(command) $ " ++ command ++ "\n" ++
  stdout ++ -- seeOutput "(stdout) " stdout ++
  seeOutput "(stderr) " stderr ++
  seeFailureExit exitCode
  where
    seeOutput :: String -> String -> String
    seeOutput tag output = if output == "" then "" else
      concat [ printf "%s%s\n" tag line | line  <- lines output ]

seeFailureExit :: ExitCode -> String
seeFailureExit = \case
  ExitFailure code -> "(exit-code) " ++ show code ++ "\n"
  ExitSuccess -> ""

----------------------------------------------------------------------
-- B: build monad

data BuildRes a = FAIL | SUCC a -- isomorphic to Maybe

type Reason = String

mergeBuildRes :: (BuildRes a, BuildRes b) -> BuildRes (a,b)
mergeBuildRes = \case
  (SUCC a, SUCC b) -> SUCC (a,b)
  (FAIL, FAIL) -> FAIL
  (FAIL, SUCC{}) -> FAIL
  (SUCC{}, FAIL) -> FAIL

bfail :: Reason -> B a
bfail = BFail

parallel_ :: [B ()] -> B ()
parallel_ xs = do _ :: [()] <- parallel xs; pure ()

parallel :: [B a] -> B [a]
parallel = \case
  [] -> pure []
  [p] -> (\x->[x]) <$> p
  p:ps -> (\(x,xs) -> x:xs) <$> BPar p (parallel ps)

instance Functor B where fmap = liftM
instance Applicative B where pure = BResult . SUCC; (<*>) = ap
instance Monad B where (>>=) = BBind

data B a where
  BResult :: BuildRes a -> B a
  BBind :: B a -> (a -> B b) -> B b
  BLog :: String -> B ()
  BCatch :: B a -> B (BuildRes a)
  BFail :: Reason -> B a
  BNewSandbox :: B Dir
  BRunActionInDir :: Dir -> Action -> B ActionRes
  Execute :: X a -> B a
  BMemoKey :: Chain -> (Key -> B Digest) -> Key -> B Digest
  BMemoRule :: (Rule -> B WitMap) -> Rule -> B WitMap
  BPar :: B a -> B b -> B (a,b)
  BYield :: B ()

runB :: Config -> B () -> X FBS
runB config@Config{logMode} build0 = do
  loop build bstate0 kFinal
  where
    build = do
      initDirs config
      build0

    sandboxParent pid = makeAbsoluteDir (printf "/tmp/jbox/%s" (show pid))

    kFinal :: BState -> BuildRes () -> X FBS
    kFinal s@BState{runCounter,jobs} _res = do
      when (length jobs /= 0) $ error "runB: unexpected left over jobs"
      myPid <- XIO getCurrentPid
      XRemoveDirRecursive (sandboxParent myPid)
      let see = case logMode of LogQuiet -> False; _ -> True
      when (see && runCounter>0) $ do XLog (printf "ran %s" (pluralize runCounter "command"))
      pure (makeFBS s)

    loop :: B a -> BState -> (BState -> BuildRes a -> X FBS) -> X FBS
    loop m0 s k = case m0 of
      BResult res -> k s res
      BCatch m -> loop m s $ \s res -> k s (SUCC res)

      BFail reason -> do
        k s { fails = reason : fails s } FAIL

      BBind m f -> loop m s $ \s -> \case
        FAIL -> k s FAIL -- cant continue; rhs 'f' ignored
        SUCC a -> loop (f a) s k

      BLog mes -> do XLog mes; k s (SUCC ())

      BNewSandbox -> do
        myPid <- XIO getCurrentPid
        let BState{sandboxCounter=i} = s
        k s { sandboxCounter = 1 + i } (SUCC (insistLocIsDir (sandboxParent myPid </> show i)))

      BRunActionInDir sandbox action@Action{commands} -> do
        res <- XRunActionInDir sandbox action
        k s { runCounter = runCounter s + length commands } (SUCC res)

      Execute x -> do a <- x; k s (SUCC a)

      BMemoKey chain f key -> do
        case Map.lookup key (memoT s) of
          Just WIP -> do
            case key `elemChain` chain of
              True -> do
                loop (bfail (printf "CYCLE: %s" (ppChain config (pushChain key chain)))) s k
              False ->
                yield s $ \s _ -> do loop m0 s k

          Just (Ready res) -> k s res
          Nothing -> do
            loop (f key) s { memoT = Map.insert key WIP (memoT s)} $ \s res -> do
              k s { memoT = Map.insert key (Ready res) (memoT s) } res

      BMemoRule f rule@Rule{target} -> do
        let arts = case target of Artifacts arts -> arts ; Phony{} -> error "BMemoRule/Phony"
        let targets = [ key | Artifact { key } <- arts ]
        case Map.lookup targets (memoR s) of
          Just WIP -> yield s $ \s _ -> do loop m0 s k
          Just (Ready res) -> k s res
          Nothing -> do
            loop (f rule) s { memoR = Map.insert targets WIP (memoR s)} $ \s res -> do
              k s { memoR = Map.insert targets (Ready res) (memoR s) } res

      BPar a b -> do
        -- continutation k is called only when both sides have completed (succ or fail)
        varA <- XIO (newIORef Nothing)
        varB <- XIO (newIORef Nothing)
        let
          kA s a = do
            XIO (readIORef varB) >>= \case
              Nothing -> do
                XIO (writeIORef varA (Just a)); next s
              Just b -> do
                k s (mergeBuildRes (a,b))
        let
          kB s b = do
            XIO (readIORef varA) >>= \case
              Nothing -> do
                XIO (writeIORef varB (Just b)); next s
              Just a -> do
                k s (mergeBuildRes (a,b))

        loop a s { jobs = BJob b kB : jobs s } kA

      BYield -> yield s k

    yield :: BState -> (BState -> BuildRes () -> X FBS) -> X FBS
    yield s k = do
      let BState{jobs} = s
      let me = BJob (pure ()) k
      next s { jobs = jobs ++ [me] }

    next :: BState -> X FBS
    next s@BState{jobs} = do
      case jobs of
        [] -> error "next: no more jobs"
        j:jobs -> resume j s { jobs }

    resume :: BJob -> BState -> X FBS
    resume (BJob p k) s = loop p s k

initDirs :: Config -> B ()
initDirs config = do
  Execute $ do
    XMakeDir (cacheFilesDir config)
    XMakeDir (tracesDir config)

data BState = BState
  { sandboxCounter :: Int
  , runCounter :: Int -- number of commands run
  , memoT :: Map Key (OrWIP (BuildRes Digest))
  , memoR :: Map [Key] (OrWIP (BuildRes WitMap))
  , jobs :: [BJob]
  , fails :: [Reason]
  }

data OrWIP a = WIP | Ready a

bstate0 :: BState
bstate0 = BState
  { sandboxCounter = 0
  , runCounter = 0
  , memoT = Map.empty
  , memoR = Map.empty
  , jobs = []
  , fails = []
  }

data BJob where
  BJob :: B a -> (BState -> BuildRes a -> X FBS) -> BJob

----------------------------------------------------------------------
-- FBS : FinalBuildState

data FBS = FBS
  { countRules :: Int
  , failures :: [Reason]
  , lookFBS :: Key -> Maybe Digest
  }

makeFBS :: BState -> FBS
makeFBS BState{memoR,memoT,fails} =
  FBS { countRules = Map.size memoR
      , failures = fails
      , lookFBS
      }
  where
    lookFBS :: Key -> Maybe Digest
    lookFBS key =
      case Map.lookup key memoT of
        Nothing -> Nothing
        Just WIP -> error "lookFBS/WIP" -- Nothing -- or is this possible?
        Just (Ready (FAIL{})) -> Nothing
        Just (Ready (SUCC digest)) -> Just digest

----------------------------------------------------------------------
-- Chain -- dependency chain for detecting cycles

data Chain = XCHAIN [Key]

emptyChain :: Chain
emptyChain = XCHAIN []

pushChain :: Key -> Chain -> Chain
pushChain key (XCHAIN keys) = XCHAIN (key:keys)

elemChain :: Key -> Chain -> Bool
elemChain key (XCHAIN keys) = key `elem` keys

ppChain :: Config -> Chain -> String
ppChain config (XCHAIN keys) = intercalate " " [ ppKey config k | k <- reverse keys ]

----------------------------------------------------------------------
-- X: execution monad

instance Functor X where fmap = liftM
instance Applicative X where pure = XRet; (<*>) = ap
instance Monad X where (>>=) = XBind

data X a where
  XRet :: a -> X a
  XBind :: X a -> (a -> X b) -> X b
  XLog :: String -> X ()
  XLogErr :: String -> X ()
  XIO :: IO a -> X a

  XRunActionInDir :: Dir -> Action -> X ActionRes
  XDigest :: Loc -> X Digest

  XFileExists :: Loc -> X Bool
  XIsSymbolicLink :: Loc -> X Bool
  XIsDirectory :: Loc -> X Bool
  XListDir :: Loc -> X [String]

  XMakeDir :: Dir -> X ()
  XCopyFile :: Loc -> Loc -> X ()
  XTransferFileMode :: Loc -> Loc -> X ()
  XMakeReadOnly :: Loc -> X ()
  XReadFile :: Loc -> X String
  XWriteFile :: String -> Loc -> X ()
  XTryHardLink :: Loc -> Loc -> X (Maybe String)
  XUnLink :: Loc -> X ()
  XRemoveDirRecursive :: Dir -> X ()

runX :: Config -> X a -> IO a
runX config@Config{homeDir,logMode,debugExternal,debugInternal,debugLocking} = loop
  where

    log mes = maybePrefixPid config mes >>= putOut

    logI :: String -> IO ()
    logI mes = when debugInternal $ log (printf "I: %s" mes)

    shell :: String -> CreateProcess
    shell s =
       (System.Process.shell s) { env = Just [ ("PATH","/usr/bin")
                                             , ("HOME",pathOfDir homeDir)
                                             ] }

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XIO io -> io
      XLog mes -> log mes
      XLogErr mes -> maybePrefixPid config mes >>= putErr

      -- sandboxed execution of user's action; for now always a list of bash commands
      XRunActionInDir dir Action{commands} -> ActionRes <$> do
        forM commands $ \command -> do
          let logAction = case logMode of LogActions -> True; _ -> False
          when logAction $ log $ printf "A: %s" command
          (exitCode,stdout,stderr) <-
            withCurrentDirectory (pathOfDir dir) (readCreateProcessWithExitCode (shell command) "")
          pure $ CommandRes { exitCode, stdout, stderr }

      -- other commands with shell out to external process
      XDigest loc -> do
        let command = printf "md5sum %s" (pathOfLoc loc)
        when debugExternal $ log (printf "X: %s" command)
        output <- readCreateProcess (shell command) ""
        let str = case (splitOn " " output) of [] -> error "XDigest/split"; x:_ -> x
        pure (Digest str)

      -- internal file system access (log approx equivalent external command)

      XFileExists loc -> do
        logI $ printf "test -e %s" (pathOfLoc loc)
        safeFileExist (pathOfLoc loc)
      XIsSymbolicLink loc -> do
        logI $ printf "test -L %s" (pathOfLoc loc)
        status <- getSymbolicLinkStatus (pathOfLoc loc)
        pure (isSymbolicLink status)
      XIsDirectory loc -> do
        logI $ printf "test -d %s" (pathOfLoc loc)
        safeFileExist (pathOfLoc loc) >>= \case
          False -> pure False
          True -> do
            status <- getFileStatus (pathOfLoc loc)
            pure (isDirectory status)
      XListDir loc -> do
        logI $ printf "ls %s" (pathOfLoc loc)
        safeListDirectory (pathOfLoc loc)

      XMakeDir dir -> do
        logI $ printf "mkdir -p %s" (pathOfDir dir)
        createDirectoryIfMissing True (pathOfDir dir)
      XCopyFile src dest -> do
        logI $ printf "cp %s %s" (pathOfLoc src) (pathOfLoc dest)
        copyFile (pathOfLoc src) (pathOfLoc dest)
      XTransferFileMode src dest -> do
        logI $ printf "cp-perms %s %s" (pathOfLoc src) (pathOfLoc dest)
        mode <- fileMode <$> getFileStatus (pathOfLoc src)
        setFileMode (pathOfLoc dest) mode
      XMakeReadOnly loc -> do
        logI $ printf "chmod a-w %s" (pathOfLoc loc)
        mode <- fileMode <$> getFileStatus (pathOfLoc loc)
        setFileMode (pathOfLoc loc) (intersectFileModes 0o555 mode)
      XReadFile loc -> do
        logI $ printf "cat %s" (pathOfLoc loc)
        readFile (pathOfLoc loc)
      XWriteFile contents dest -> do
        let mes :: String = printf "cat> %s" (pathOfLoc dest)
        when debugLocking $ log (printf "L: %s" mes)
        writeFileFlush (pathOfLoc dest) contents
      XTryHardLink src dest -> do
        logI $ printf "ln %s %s" (pathOfLoc src) (pathOfLoc dest)
        tryCreateLink (pathOfLoc src) (pathOfLoc dest)
      XUnLink loc -> do
        logI $ printf "rm -f %s" (pathOfLoc loc)
        safeRemoveLink (pathOfLoc loc)
      XRemoveDirRecursive dir -> do
        logI $ printf "rm -rf %s" (pathOfDir dir)
        removePathForcibly (pathOfDir dir)

writeFileFlush :: FilePath -> String -> IO ()
writeFileFlush fp str = do
  withFile fp WriteMode $ \h -> do hPutStr h str; hFlush h

safeFileExist :: FilePath -> IO Bool
safeFileExist fp0 = do
  let !fp = fp0
  try (fileExist fp) >>= \case
    Right b -> pure b
    Left (_e::SomeException) -> do
      --putErr ("safeFileExist: " ++ show _e)
      pure False

tryCreateLink :: FilePath -> FilePath -> IO (Maybe String)
tryCreateLink a0 b0 = do
  let !a = a0
  let !b = b0
  try (createLink a b) >>= \case
    Right () -> pure Nothing
    Left (_e::SomeException) -> do
      --putErr ("tryCreateLink: " ++ show _e)
      pure (Just (show _e))

safeRemoveLink :: FilePath -> IO ()
safeRemoveLink fp0 = do
  let !fp = fp0
  try (removeLink fp) >>= \case
    Right () -> pure ()
    Left (_e::SomeException) -> do
      --putErr ("safeRemoveLink: " ++ show _e)
      pure ()

safeListDirectory :: FilePath -> IO [String]
safeListDirectory fp0 = do
  let !fp = fp0
  try (listDirectory fp) >>= \case
    Right xs -> pure xs
    Left (_e::SomeException) -> do
      --putErr ("safeListDirectory: " ++ show _e)
      pure []

maybePrefixPid :: Config -> String -> IO String
maybePrefixPid Config{seePid} mes = do
  case seePid of
    False -> pure mes
    True -> do
      myPid <- getCurrentPid
      pure $ printf "[%s] %s" (show myPid) mes

putErr :: String -> IO ()
putErr s = do
  hPutStrLn IO.stderr s
  hFlush IO.stderr
  pure ()

putOut :: String -> IO ()
putOut s = do
  hPutStrLn IO.stdout s
  hFlush IO.stdout
  pure ()
