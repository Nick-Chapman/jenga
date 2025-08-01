module Engine (engineMain) where

import CommandLine (LogMode(..),Config(..),BuildMode(..),CacheDirSpec(..))
import CommandLine qualified (exec)
import Control.Exception (try,SomeException)
import Control.Monad (ap,liftM)
import Control.Monad (when,forM,forM_)
import Data.Hash.MD5 qualified as MD5
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (intercalate)
import Data.List qualified as List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Interface (G(..),D(..),Rule(..),Action(..),Key(..),Loc(..),What(..))
import StdBuildUtils ((</>),dirLoc)
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile,getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit(ExitCode(..))
import System.FileLock (tryLockFile,SharedExclusive(Exclusive),unlockFile)
import System.FilePath qualified as FP
import System.IO (hFlush,hPutStrLn,withFile,IOMode(WriteMode),hPutStr)
import System.IO qualified as IO (stderr,stdout)
import System.Posix.Files (fileExist,createLink,removeLink,getFileStatus,getSymbolicLinkStatus,fileMode,intersectFileModes,setFileMode,getFileStatus,isDirectory,isSymbolicLink)
import System.Posix.Process (forkProcess)
import System.Process (shell,callProcess,readCreateProcess,readCreateProcessWithExitCode,getCurrentPid)
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.IO.SafeWrite (syncFile)
import WaitPid(waitpid)

----------------------------------------------------------------------
-- Engine main

type UserProg = [String] -> G ()

engineMain :: UserProg -> IO ()
engineMain userProg = do
  config@Config{cacheDirSpec,logMode} <- CommandLine.exec
  let quiet = case logMode of LogQuiet -> True; _ -> False
  myPid <- getCurrentPid

  cacheDir <-
    case cacheDirSpec of
      CacheDirDefault -> do
        lookupEnv "XDG_CACHE_HOME" >>= \case
          Just cache -> do
            pure (Loc cache </> "jenga")
          Nothing -> do
            home <- getHomeDirectory
            pure (Loc home </> ".cache/jenga")
      CacheDirChosen dir -> do
        pure (Loc dir  </> ".cache/jenga")
      CacheDirTemp -> do
        let loc = Loc (printf "/tmp/.cache/jenga/%s" (show myPid))
        when (not quiet) $ putOut (printf "using temporary cache: %s" (show loc))
        pure loc

  elaborateAndBuild cacheDir config userProg


elaborateAndBuild :: Loc -> Config -> UserProg -> IO ()
elaborateAndBuild cacheDir config@Config{buildMode,args} userProg = do
  case buildMode of

    ModeListTargets -> do
      runBuild cacheDir config $ \config -> do
        system <- runElaboration config (userProg args)
        let System{rules} = system
        let allTargets = [ target | Rule{hidden,targets} <- rules, target <- targets, not hidden ]
        sequence_ [ BLog (show key) | key <- allTargets ]

    ModeListRules -> do
      runBuild cacheDir config $ \config -> do
        system <- runElaboration config (userProg args)
        let System{how,rules} = system
        staticRules :: [StaticRule] <- concat <$>
          sequence [ do (deps,action@Action{hidden=actionHidden}) <- gatherDeps config how depcom
                        pure [ StaticRule { rulename, dir, targets, deps, action }
                             | not actionHidden
                             ]
                   | Rule{rulename,dir,hidden=ruleHidden,targets,depcom} <- rules
                   , not ruleHidden
                   ]
        BLog (intercalate "\n\n" (map show staticRules))

    ModeBuild -> do
      runBuild cacheDir config $ \config -> do
        system <- runElaboration config (userProg args)
        buildWithSystem config system
        reportSystem config system

    ModeBuildAndRun target argsForTarget -> do
      runBuild cacheDir config $ \config -> do
        system <- runElaboration config (userProg [FP.takeDirectory target])
        buildWithSystem config system
        reportSystem config system
        Execute (XIO (callProcess (printf ",jenga/%s" target) argsForTarget))

runBuild :: Loc -> Config -> (Config -> B ()) -> IO ()
runBuild cacheDir config f = do
  nCopies config $ \config -> do
    runX config $ do
      runB cacheDir config (f config)

nCopies :: Config -> (Config -> IO ()) -> IO ()
nCopies config@Config{jnum} f =
  if jnum < 1 then error "nCopies<1" else do
    children <- sequence $ replicate (jnum-1) $ forkProcess (f $ config { worker = True })
    f config -- parent
    mapM_ waitpid children

buildWithSystem :: Config -> System -> B ()
buildWithSystem config system = do
  let System{rules,how} = system
  let allTargets = [ target | Rule{hidden,targets} <- rules, target <- targets, not hidden ]
  _ :: [()] <- parallel [ buildAndMaterialize config how key
                        | key <- allTargets
                        ]
  pure ()

data StaticRule = StaticRule
  { rulename :: String
  , dir :: Loc
  , targets :: [Key]
  , deps :: [Key]
  , action :: Action
  }

instance Show StaticRule where
  show StaticRule{dir,targets,deps,action=Action{commands}} = do
    let cdPrefix = if dir == Loc "." then "" else printf "cd %s ; " (show dir)
    let sep = printf "\n  %s" cdPrefix
    printf "%s : %s%s%s" (seeKeys targets) (seeKeys deps) sep (intercalate sep commands)

seeKeys :: [Key] -> String
seeKeys = intercalate " " . map show

pluralize :: Int -> String -> String
pluralize n what = printf "%d %s%s" n what (if n == 1 then "" else "s")

reportSystem :: Config -> System -> B ()
reportSystem Config{logMode,worker} System{rules} = do
  let quiet = case logMode of LogQuiet -> True; _ -> False
  let nTargets = sum [ length targets |  Rule{targets,hidden} <- rules, not hidden ]
  when (not quiet && not worker) $ BLog $ printf "checked %s" (pluralize nTargets "target")

buildAndMaterialize :: Config -> How -> Key -> B ()
buildAndMaterialize config how key = do
  digest <- buildTarget config how key
  materialize digest key
  pure ()

materialize :: Digest -> Key -> B ()
materialize digest key = do
  cacheFile <- cacheFile digest
  let materializedFile = artifactsDir </> show key
  Execute $ do
    XMakeDir (dirLoc materializedFile)
    XTryHardLink cacheFile materializedFile >>= \case
      Nothing -> pure ()
      Just{} -> do
        -- Likely from concurrently runnning jenga
        -- If we do materialization when (just after) the action is run, this wont happen.
        -- XLog (printf "Materialize/HardLink: we lost the race: %s -> %s" (show cacheFile) (show materializedFile))
        pure ()

----------------------------------------------------------------------
-- locations for cache, sandbox etc

cachedFilesDir,tracesDir :: B Loc
cachedFilesDir = do cacheDir <- BCacheDir; pure (cacheDir </> "files")
tracesDir = do cacheDir <- BCacheDir; pure (cacheDir </> "traces")

artifactsDir :: Loc
artifactsDir = Loc ",jenga"

----------------------------------------------------------------------
-- Elaborate

data System = System { rules :: [Rule], how :: How }

type How = Map Key Rule

runElaboration :: Config -> G () -> B System
runElaboration config m =
  loop m system0 k0 >>= \case
    system -> do
      pure system

  where
    system0 :: System
    system0 = System { rules = [], how = Map.empty }

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
        let Rule{targets} = rule
        xs <- sequence [ do b <- Execute (XFileExists loc); pure (key,b)
                        | key@(Key loc) <- targets ]
        case [ key | (key,isSource) <- xs, isSource ] of
          clashS@(_:_) -> do
            bfail $ printf "rule targets clash with source :%s" (show clashS)
          [] -> do
            let System{rules,how} = system
            case filter (flip Map.member how) targets of
              clashR@(_:_) -> do
                bfail $ printf "rule targets defined by earlier rule :%s" (show clashR)
              [] -> do
                how <- pure $ List.foldl' (flip (flip Map.insert rule)) how targets
                k system { rules = rule : rules, how } ()

      GWhat loc -> do
        Execute (xwhat loc) >>= k system

      GReadKey key -> do
        let System{how} = system
        contents <- readKey config how key -- make cause building
        k system contents

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

locateKey :: Key -> Loc
locateKey (Key (Loc fp)) = Loc (FP.takeFileName fp)

----------------------------------------------------------------------
-- Build

buildTarget :: Config -> How -> Key -> B Digest
buildTarget config@Config{debugDemand} how = do
  -- TODO: document this flow.
  -- TODO: check for cycles.
  BMemoKey $ \sought -> do
    case Map.lookup sought how of
      Nothing -> do
        let Key loc = sought
        Execute (XFileExists loc) >>= \case
          False -> do
            bfail (printf "'%s' : is not source and has no build rule" (show sought))
          True -> do
            digest <- copyIntoCache loc
            pure digest

      Just rule -> do
        when debugDemand $ BLog (printf "B: Require: %s" (show sought))
        wtargets <- buildRule config how rule
        let digest = lookWitMap (locateKey sought) wtargets
        pure digest

buildRule :: Config -> How -> Rule -> B WitMap
buildRule config@Config{debugLocking} how =
  BMemoRule $ \rule -> do
    let Rule{targets,depcom} = rule
    (deps,action@Action{commands}) <- gatherDeps config how depcom
    wdeps <- (WitMap . Map.fromList) <$>
      parallel [ do digest <- buildTarget config how dep; pure (locateKey dep,digest)
               | dep <- deps
               ]
    let witKey = WitnessKey { targets = map locateKey targets, commands, wdeps }
    let wkd = digestWitnessKey witKey
    -- If the witness trace already exists, use it.
    when debugLocking $ Execute $ XLog (printf "L: looking for witness: %s" (show wkd))
    verifyWitness config wkd rule >>= \case
      Just w -> pure w
      Nothing -> do
        -- If not, someone has to run the job...
        when debugLocking $ Execute $ XLog (printf "L: no witness; job must be run: %s" (show wkd))
        tryGainingLock debugLocking wkd $ \case
          True -> do
            -- We got the lock, check again for the trace...
            when debugLocking $ Execute $ XLog (printf "L: look again for witness: %s" (show wkd))
            verifyWitness config wkd rule >>= \case
              Just w -> pure w
              Nothing -> do
                when debugLocking $ Execute $ XLog (printf "L: still no witness; running jobs NOW: %s" (show wkd))
                -- We have the lock and there is still no trace, so we run the job....
                runActionSaveWitness config action wkd wdeps rule
          False -> do
            when debugLocking $ Execute $ XLog (printf "L: running elsewhere: %s" (show wkd))
            -- Another process beat us to the lock; it's running the job.
            awaitLockYielding config wkd
            -- Now it will have finished.
            verifyWitness config wkd rule >>= \case
              Just w -> pure w
              Nothing -> BResult (FAIL [])

verifyWitness :: Config -> WitKeyDigest -> Rule -> B (Maybe WitMap)
verifyWitness config wkd rule = do
  lookupWitness wkd >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      case wit of
        WitnessFAIL ares -> do
          Execute (showActionRes config ares)
          actionFailed rule
        WitnessSUCC{wtargets,ares} -> do
          let WitMap m = wtargets
          ok <- all id <$> sequence [ existsCacheFile digest | (_,digest) <- Map.toList m ]
          if not ok then pure Nothing else do
            Execute (showActionRes config ares)
            pure (Just wtargets)

awaitLockYielding :: Config -> WitKeyDigest -> B ()
awaitLockYielding Config{debugLocking} wkd = loop 0
  where
    loop :: Int -> B ()
    loop i = do
      BYield
      tryGainingLock False wkd $ \case
        False -> loop (i+1)
        True -> do
          when debugLocking $ Execute $ XLog (printf "L: spun (%d) awaiting lock %s" i (show wkd))
          pure ()

tryGainingLock :: Bool -> WitKeyDigest -> (Bool -> B a) -> B a
tryGainingLock debug wkd f = do
  tracesDir <- tracesDir
  let Loc lockPath = tracesDir </> (show wkd ++ ".lock")
  -- Try to gain the lock...
  Execute (XIO (tryLockFile lockPath Exclusive)) >>= \case
    Just lock -> do
      when debug $ Execute $ XLog (printf "L: locked: %s" (show wkd))
      -- Run the critical section while the lock is held.
      res <- BCatch (f True)
      -- Critical section finished; release the lock (unlink and unlok).
      Execute $ do
        XUnLink (Loc lockPath)
        XIO (unlockFile lock)
        when debug $ XLog $ printf "L: unlocked %s" (show wkd)
      BResult res
    Nothing ->
      -- We lost the race; another process has the lock.
      f False


gatherDeps :: Config -> How -> D a -> B ([Key],a)
gatherDeps config how d = loop d [] k0
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
        contents <- readKey config how key
        k xs contents
      DExistsKey key -> do
        b <- existsKey how key
        k xs b

readKey :: Config -> How -> Key -> B String
readKey config how key = do
  digest <- buildTarget config how key
  file <- cacheFile digest
  Execute (XReadFile file)

existsKey :: How -> Key -> B Bool
existsKey how key =
  if Map.member key how
  then pure True
  else do
    let Key loc = key
    Execute (XFileExists loc)

runActionSaveWitness :: Config -> Action -> WitKeyDigest -> WitMap -> Rule -> B WitMap
runActionSaveWitness config action wkd depWit rule = do
  sandbox <- BNewSandbox
  Execute (XMakeDir sandbox)
  setupInputs sandbox depWit
  ares <- BRunActionInDir sandbox action
  Execute (showActionRes config ares)
  let ok = actionResIsOk ares
  case ok of
    False -> do
      let wit = WitnessFAIL ares
      saveWitness wkd wit
      actionFailed rule
    True -> do
      wtargets <- cacheOutputs sandbox rule -- If this fails we dont write a witness. TODO: is that right?
      let wit = WitnessSUCC { wtargets, ares }
      saveWitness wkd wit
      pure wtargets

actionFailed :: Rule -> B a
actionFailed rule = do
  let Rule{targets,rulename} = rule
  bfail (printf "'%s': action failed for rule '%s'" (intercalate " " $ map show targets) rulename)

setupInputs :: Loc -> WitMap -> B ()
setupInputs sandbox (WitMap m1) = do
  sequence_
    [ do
        src <- cacheFile digest
        let dest = sandbox </> show loc
        Execute $ hardLinkRetry1 src dest
    | (loc,digest) <- Map.toList m1
    ]

hardLinkRetry1 :: Loc -> Loc -> X ()
hardLinkRetry1 src dest =  do
  XTryHardLink src dest >>= \case
    Nothing -> pure ()
    Just _e1 -> do
      -- Sometimes the hardlink fails; but it works on retry. Why?
      -- XLogErr _e1
      XTryHardLink src dest >>= \case
        Nothing -> pure ()
        Just e2 ->
          -- TODO: I don't think we can assume we only need a single retry
          error e2 -- hard error for error on 2nd attempt

cacheOutputs :: Loc -> Rule -> B WitMap
cacheOutputs sandbox Rule{rulename,targets} = do
  WitMap . Map.fromList <$> sequence
    [ do
        let tag = locateKey target
        let sandboxLoc = sandbox </> show tag
        Execute (XFileExists sandboxLoc) >>= \case
          False -> do
            bfail (printf "'%s' : not produced as declared by rule '%s'" (show target) rulename)
          True -> do
            digest <- linkIntoCache sandboxLoc
            pure (tag,digest)
    | target <- targets
    ]

copyIntoCache :: Loc -> B Digest
copyIntoCache loc = do
  digest <- Execute (XDigest loc)
  file <- cacheFile digest
  Execute (XFileExists file) >>= \case
    True -> pure ()
    False -> do
      Execute $ do
        XCopyFile loc file
        XMakeReadOnly file
        pure ()

  pure digest

linkIntoCache :: Loc -> B Digest
linkIntoCache loc = do
  digest <- Execute (XDigest loc)
  file <- cacheFile digest
  Execute $ do
    XFileExists file >>= \case
      True -> XTransferFileMode loc file
      False -> do
        XTryHardLink loc file >>= \case
          Nothing -> pure ()
          Just err -> do
            -- job locking ensures this can't happen
            error (printf "linkIntoCache/HardLink: failure: loc=%s, file=%s, err=%s"
                   (show loc) (show file) (show err)
                  )
    XMakeReadOnly file
  pure digest

cacheFile :: Digest -> B Loc
cacheFile (Digest str) = do
  cachedFilesDir <- cachedFilesDir
  pure (cachedFilesDir </> str)

-- message digest of a file; computed by call to external md5sum
data Digest = Digest String

instance Show Digest where show (Digest str) = str

----------------------------------------------------------------------
-- Build witnesses (AKA constructive traces)

type Witness = WitnessValue

data WitnessValue
  = WitnessSUCC { wtargets :: WitMap, ares :: ActionRes }
  | WitnessFAIL ActionRes

data WitnessKey = WitnessKey { targets :: [Loc], commands :: [String], wdeps :: WitMap } deriving Show

-- TODO: perhaps filemode should be included in the target WitMap
data WitMap = WitMap (Map Loc Digest) deriving Show

-- message digest of a witness key; computer by internal MD5 code
data WitKeyDigest = WitKeyDigest String

instance Show WitKeyDigest where show (WitKeyDigest str) = str

lookWitMap :: Loc -> WitMap -> Digest
lookWitMap loc (WitMap m) = maybe err id $ Map.lookup loc m
  where err = error (show ("lookWitMap",loc,Map.keys m))

digestWitnessKey :: WitnessKey -> WitKeyDigest
digestWitnessKey wk = WitKeyDigest (MD5.md5s (MD5.Str (show wk)))

lookupWitness :: WitKeyDigest -> B (Maybe Witness)
lookupWitness wkd = do
  witFile <- witnessFile wkd
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

existsCacheFile :: Digest -> B Bool
existsCacheFile digest = do
  file <- cacheFile digest
  Execute (XFileExists file)

saveWitness :: WitKeyDigest -> Witness -> B ()
saveWitness wkd wit = do
  witFile <- witnessFile wkd
  Execute $ do
    XWriteFile (exportWitness wit ++ "\n") witFile

witnessFile :: WitKeyDigest -> B Loc
witnessFile wkd = do
  tracesDir <- tracesDir
  pure (tracesDir </> show wkd)

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

type QWitMap = [(FilePath,QDigest)]
type QDigest = String

toQ :: Witness -> QWitness
toQ = \case
  WitnessFAIL (ActionRes xs) -> do
    let cs = [ RUN{exitCode,stdout,stderr} | CommandRes{exitCode,stdout,stderr} <- xs ]
    TRACE cs Nothing
  WitnessSUCC{wtargets=WitMap m,ares=ActionRes xs} -> do
    let cs = [ RUN{exitCode,stdout,stderr} | CommandRes{exitCode,stdout,stderr} <- xs ]
    let targets = [ (fp,digest) | (Loc fp,Digest digest) <- Map.toList m ]
    TRACE cs (Just targets)

fromQ :: QWitness -> Witness
fromQ = \case
  TRACE cs topt -> do
    let ares = ActionRes [ CommandRes{exitCode,stdout,stderr} | RUN{exitCode,stdout,stderr} <- cs ]
    case topt of
      Nothing -> WitnessFAIL ares
      Just targets -> do
        let wtargets = WitMap (Map.fromList [ (Loc fp,Digest digest) | (fp,digest) <- targets ])
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

showActionRes :: Config -> ActionRes -> X ()
showActionRes Config{worker} (ActionRes xs) = do
  when (not worker) $ do
    XIO $ do
      forM_ xs $ \CommandRes{exitCode,stdout,stderr} -> do
        -- TODO: distinuish stdout/stderr? link to rule?
        putStr stdout
        putStr stderr
        let isFailure = \case ExitFailure{} -> True; ExitSuccess -> False
        when (isFailure exitCode) $ putStrLn (show exitCode)

----------------------------------------------------------------------
-- B: build monad

data BuildRes a = FAIL [Reason] | SUCC a

type Reason = String

mergeBuildRes :: (BuildRes a, BuildRes b) -> BuildRes (a,b)
mergeBuildRes = \case
  (SUCC a, SUCC b) -> SUCC (a,b)
  (FAIL reasons1, FAIL reasons2) -> FAIL (reasons1 ++ reasons2)
  (FAIL reasons, SUCC{}) -> FAIL reasons
  (SUCC{}, FAIL reasons) -> FAIL reasons

removeReasons :: BuildRes a -> BuildRes a -- TODO: think
removeReasons = \case
  succ@SUCC{} -> succ
  FAIL{}  -> FAIL[]

bfail :: Reason -> B a
bfail r1 = BResult (FAIL [r1])

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
  BCacheDir :: B Loc
  BNewSandbox :: B Loc
  BRunActionInDir :: Loc -> Action -> B ActionRes
  Execute :: X a -> B a
  BMemoKey :: (Key -> B Digest) -> Key -> B Digest
  BMemoRule :: (Rule -> B WitMap) -> Rule -> B WitMap
  BPar :: B a -> B b -> B (a,b)
  BYield :: B ()

runB :: Loc -> Config -> B () -> X ()
runB cacheDir config@Config{logMode} build0 = do
  loop build bstate0 kFinal
  where
    build = do
      initDirs
      build0

    sandboxParent pid = Loc (printf "/tmp/.jbox/%s" (show pid))

    kFinal :: BState -> BuildRes () -> X ()
    kFinal BState{runCounter,jobs} res = do
      when (length jobs /= 0) $ error "runB: unexpected left over jobs"
      myPid <- XIO getCurrentPid
      XRemoveDirRecursive (sandboxParent myPid)
      let see = case logMode of LogQuiet -> False; _ -> True
      let i = runCounter
      when (see && i>0) $ XLog (printf "ran %s" (pluralize i "command"))
      reportBuildRes config res

    loop :: B a -> BState -> (BState -> BuildRes a -> X ()) -> X ()
    loop m0 s k = case m0 of
      BResult res -> k s res
      BCatch m -> loop m s $ \s res -> k s (SUCC res)

      BBind m f -> loop m s $ \s -> \case
        FAIL reasons -> k s (FAIL reasons) -- cant continue; rhs 'f' ignored
        SUCC a -> loop (f a) s k

      BLog mes -> do XLog mes; k s (SUCC ())

      BCacheDir -> k s (SUCC cacheDir)
      BNewSandbox -> do
        myPid <- XIO getCurrentPid
        let BState{sandboxCounter=i} = s
        k s { sandboxCounter = 1 + i } (SUCC (sandboxParent myPid </> show i))

      BRunActionInDir sandbox action@Action{hidden,commands} -> do
        res <- XRunActionInDir sandbox action
        k s { runCounter = runCounter s + (if hidden then 0 else length commands)} (SUCC res)

      Execute x -> do a <- x; k s (SUCC a)

      BMemoKey f key -> do
        case Map.lookup key (memoT s) of
          Just WIP -> yield s $ \s _ -> do loop m0 s k
          Just (Ready res) -> k s (removeReasons res)
          Nothing -> do
            loop (f key) s { memoT = Map.insert key WIP (memoT s)} $ \s res -> do
              k s { memoT = Map.insert key (Ready res) (memoT s) } res

      BMemoRule f rule@Rule{targets} -> do
        case Map.lookup targets (memoR s) of
          Just res -> k s (removeReasons res)
          Nothing -> do
            loop (f rule) s $ \s res -> do
              k s { memoR = Map.insert targets res (memoR s) } res


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

    yield :: BState -> (BState -> BuildRes () -> X ()) -> X ()
    yield s k = do
      let BState{jobs} = s
      let me = BJob (pure ()) k
      next s { jobs = jobs ++ [me] }

    next :: BState -> X ()
    next s@BState{jobs} = do
      case jobs of
        [] -> error "next: no more jobs"
        j:jobs -> resume j s { jobs }

    resume :: BJob -> BState -> X ()
    resume (BJob p k) s = loop p s k

initDirs :: B ()
initDirs = do
  tracesDir <- tracesDir
  cachedFilesDir <- cachedFilesDir
  Execute $ do
    XRemoveDirRecursive artifactsDir
    XMakeDir cachedFilesDir
    XMakeDir tracesDir
    XMakeDir artifactsDir

reportBuildRes :: Config -> BuildRes () -> X ()
reportBuildRes Config{worker} res =
  case res of
    FAIL reasons -> do
      when (not worker) $ do
        -- TODO: Sometimes see 0 reasons when use -j<NUM>
        XLog (printf "Build failed for %d reasons:\n%s" (length reasons)
              (intercalate "\n" [ printf "(%d) %s" i r | (i,r) <- zip [1::Int ..] reasons ]))
    SUCC () ->
      pure ()

data BState = BState
  { sandboxCounter :: Int
  , runCounter :: Int -- less that sandboxCounter because of hidden actions
  , memoT :: Map Key (OrWIP (BuildRes Digest))
  , memoR :: Map [Key] (BuildRes WitMap)
  , jobs :: [BJob]
  }

data OrWIP a = WIP | Ready a

bstate0 :: BState
bstate0 = BState
  { sandboxCounter = 0
  , runCounter = 0
  , memoT = Map.empty
  , memoR = Map.empty
  , jobs = []
  }

data BJob where
  BJob :: B a -> (BState -> BuildRes a -> X ()) -> BJob

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

  XRunActionInDir :: Loc -> Action -> X ActionRes
  XDigest :: Loc -> X Digest

  XFileExists :: Loc -> X Bool
  XIsSymbolicLink :: Loc -> X Bool
  XIsDirectory :: Loc -> X Bool
  XListDir :: Loc -> X [String]

  XMakeDir :: Loc -> X ()
  XCopyFile :: Loc -> Loc -> X ()
  XTransferFileMode :: Loc -> Loc -> X ()
  XMakeReadOnly :: Loc -> X ()
  XReadFile :: Loc -> X String
  XWriteFile :: String -> Loc -> X ()
  XTryHardLink :: Loc -> Loc -> X (Maybe String)
  XUnLink :: Loc -> X ()
  XRemoveDirRecursive :: Loc -> X ()

runX :: Config -> X a -> IO a
runX config@Config{logMode,debugExternal,debugInternal,debugLocking} = loop
  where

    log mes = maybePrefixPid config mes >>= putOut

    logI :: String -> IO ()
    logI mes = when debugInternal $ log (printf "I: %s" mes)

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XIO io -> io
      XLog mes -> log mes
      XLogErr mes -> maybePrefixPid config mes >>= putErr

      -- sandboxed execution of user's action; for now always a list of bash commands
      XRunActionInDir (Loc dir) Action{hidden,commands} -> ActionRes <$> do
        forM commands $ \command -> do
          let logAction = case logMode of LogActions -> True; _ -> False
          when (not hidden && logAction) $ log $ printf "A: %s" command
          (exitCode,stdout,stderr) <-
            withCurrentDirectory dir (readCreateProcessWithExitCode (shell command) "")
          pure $ CommandRes { exitCode, stdout, stderr }

      -- other commands with shell out to external process
      XDigest (Loc fp) -> do
        let command = printf "md5sum %s" fp
        when debugExternal $ log (printf "X: %s" command)
        output <- readCreateProcess (shell command) ""
        let str = case (splitOn " " output) of [] -> error "XDigest/split"; x:_ -> x
        pure (Digest str)

      -- internal file system access (log approx equivalent external command)

      XFileExists (Loc fp) -> do
        logI $ printf "test -e %s" fp
        safeFileExist fp
      XIsSymbolicLink (Loc fp) -> do
        logI $ printf "test -L %s" fp
        status <- getSymbolicLinkStatus fp
        pure (isSymbolicLink status)
      XIsDirectory (Loc fp) -> do
        logI $ printf "test -d %s" fp
        safeFileExist fp >>= \case
          False -> pure False
          True -> do
            status <- getFileStatus fp
            pure (isDirectory status)
      XListDir (Loc fp) -> do
        logI $ printf "ls %s" fp
        safeListDirectory fp

      XMakeDir (Loc fp) -> do
        logI $ printf "mkdir -p %s" fp
        createDirectoryIfMissing True fp
      XCopyFile (Loc src) (Loc dest) -> do
        logI $ printf "cp %s %s" src dest
        copyFile src dest
      XTransferFileMode (Loc src) (Loc dest) -> do
        mode <- fileMode <$> getFileStatus src
        setFileMode dest mode
      XMakeReadOnly (Loc fp) -> do
        logI $ printf "chmod a-w %s" fp
        mode <- fileMode <$> getFileStatus fp
        setFileMode fp (intersectFileModes 0o555 mode)
      XReadFile (Loc fp) -> do
        logI $ printf "cat %s" fp
        readFile fp
      XWriteFile contents (Loc dest) -> do
        let mes :: String = printf "cat> %s" dest
        when debugLocking $ log (printf "L: %s" mes)
        writeFileFlush dest contents
      XTryHardLink (Loc src) (Loc dest) -> do
        logI $ printf "ln %s %s" src dest
        tryCreateLink src dest
      XUnLink (Loc fp) -> do
        logI $ printf "rm -f %s" fp
        safeRemoveLink fp
      XRemoveDirRecursive (Loc fp) -> do
        logI $ printf "rm -rf %s" fp
        removePathForcibly fp

writeFileFlush :: FilePath -> String -> IO ()
writeFileFlush fp str = do
  withFile fp WriteMode $ \h -> do hPutStr h str; hFlush h
  -- Enabling this prevents lock racing & duplicate actions. But it is a big slow sledgehammer!
  when False $ syncFile fp
  pure ()

safeFileExist :: FilePath -> IO Bool
safeFileExist fp = do
  try (fileExist fp) >>= \case
    Right b -> pure b
    Left (_e::SomeException) -> do
      --putErr (show _e)
      pure False

tryCreateLink :: FilePath -> FilePath -> IO (Maybe String)
tryCreateLink a b = do
  try (createLink a b) >>= \case
    Right () -> pure Nothing
    Left (_e::SomeException) -> do
      --putErr (show _e)
      pure (Just (show _e))

safeRemoveLink :: FilePath -> IO ()
safeRemoveLink fp = do
  try (removeLink fp) >>= \case
    Right () -> pure ()
    Left (_e::SomeException) -> do
      --putErr (show _e)
      pure ()

safeListDirectory :: FilePath -> IO [String]
safeListDirectory fp = do
  try (listDirectory fp) >>= \case
    Right xs -> pure xs
    Left (_e::SomeException) -> do
      --putErr (show _e)
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
