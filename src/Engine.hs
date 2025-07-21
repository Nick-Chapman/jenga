module Engine (engineMain) where

import CommandLine (LogMode(..),Config(..),BuildMode(..),CacheDirSpec(..))
import CommandLine qualified (exec)
import Control.Exception (try,SomeException)
import Control.Monad (ap,liftM)
import Control.Monad (when,forM)
import Data.Hash.MD5 qualified as MD5
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (intercalate)
import Data.List qualified as List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Interface (G(..),D(..),Rule(..),Action(..),Key(..),Loc(..))
import StdBuildUtils ((</>),dirLoc)
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile,getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit(ExitCode(..))
import System.FileLock (tryLockFile,SharedExclusive(Exclusive),unlockFile)
import System.FilePath qualified as FP
import System.IO (hFlush,hPutStrLn,stderr,stdout,withFile,IOMode(WriteMode),hPutStr)
import System.Posix.Files (fileExist,createLink,removeLink,getFileStatus,fileMode,intersectFileModes,setFileMode,getFileStatus,isDirectory)
import System.Posix.Process (forkProcess)
import System.Process (shell,callProcess,readCreateProcess,readCreateProcessWithExitCode,getCurrentPid)
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.IO.SafeWrite (syncFile)

----------------------------------------------------------------------
-- Engine main

type UserProg = [String] -> G ()

engineMain :: UserProg -> IO ()
engineMain userProg = do
  config@Config{cacheDirSpec,logMode} <- CommandLine.exec
  let see = case logMode of LogQuiet -> False; _ -> True
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
        when see $ putOut (printf "using temporary cache: %s" (show loc))
        pure loc

  elaborateAndBuild cacheDir config userProg


elaborateAndBuild :: Loc -> Config -> UserProg -> IO ()
elaborateAndBuild cacheDir config@Config{buildMode,args} userProg = do
  --let see = case logMode of LogQuiet -> False; _ -> True
  case buildMode of

    ModeListTargets -> do
      runB cacheDir config $ do
        system <- runElaboration config (userProg args)
        reportSystem config system
        let System{rules} = system
        let allTargets = [ target | Rule{hidden,targets} <- rules, target <- targets, not hidden ]
        sequence_ [ BLog (show key) | key <- allTargets ]


    ModeListRules -> do
      runB cacheDir config $ do
        system <- runElaboration config (userProg args)
        reportSystem config system
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
      runB cacheDir config $ do
        system <- runElaboration config (userProg args)
        reportSystem config system
        buildWithSystem config system

    ModeBuildAndRun target argsForTarget -> do
      runB cacheDir config $ do
        system <- runElaboration config (userProg [FP.takeDirectory target])
        reportSystem config system
        buildWithSystem config system
      callProcess (printf ",jenga/%s" target) argsForTarget


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
reportSystem Config{logMode} system = do
  let see = case logMode of LogQuiet -> False; _ -> True
  let System{rules} = system
  let nRules = sum [ if hidden then 0 else 1 | Rule{hidden} <- rules ]
  let nTargets = sum [ length targets |  Rule{targets,hidden} <- rules, not hidden ]
  when see $ BLog $ printf "elaborated %s and %s" (pluralize nRules "rule") (pluralize nTargets "target")

buildAndMaterialize :: Config -> How -> Key -> B ()
buildAndMaterialize config how key = do
  digest <- doBuild config how key
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

type OrErr a = Either ErrMessage a
data ErrMessage = ErrMessage String

instance Show ErrMessage where show (ErrMessage s) = printf "Error: %s" s

data System = System { rules :: [Rule], how :: How }

type How = Map Key Rule

runElaboration :: Config -> G () -> B System
runElaboration config m =
  loop m system0 k0 >>= \case
    Left mes -> do
      error $ printf "runElaboration -> Error:\n%s\n" (show mes)
    Right system -> do
      pure system

  where
    system0 :: System
    system0 = System { rules = [], how = Map.empty }

    k0 :: System -> () -> B (OrErr System)
    k0 s () = pure (Right s)

    loop :: G a -> System -> (System -> a -> B (OrErr System)) -> B (OrErr System)
    loop m system k = case m of
      GRet a -> k system a
      GBind m f -> loop m system $ \system a -> loop (f a) system k
      GLog mes -> do
        Execute (XLog (printf "log: %s" mes))
        k system ()
      GFail mes -> pure (Left (ErrMessage mes)) -- ignore k
      GRule rule -> do
        let Rule{targets} = rule
        xs <- sequence [ do b <- Execute (XFileExists loc); pure (key,b)
                        | key@(Key loc) <- targets ]
        case [ key | (key,isSource) <- xs, isSource ] of
          clashS@(_:_) -> do
            let mes = printf "rule targets clash with source :%s" (show clashS)
            pure (Left (ErrMessage mes))
          [] -> do
            let System{rules,how} = system
            case filter (flip Map.member how) targets of
              clashR@(_:_) -> do
                let mes = printf "rule targets defined by previous rule :%s" (show clashR)
                pure (Left (ErrMessage mes))
              [] -> do
                how <- pure $ List.foldl' (flip (flip Map.insert rule)) how targets
                k system { rules = rule : rules, how } ()

      GGlob dir -> do
        locs <- Execute (XGlob dir)
        k system locs
      GExists loc -> do
        bool <- Execute (XFileExists loc)
        k system bool
      GIsDirectory loc -> do
        bool <- Execute (XIsdirectory loc)
        k system bool
      GReadKey key -> do
        let System{how} = system
        contents <- readKey config how key -- make cause building
        k system contents

locateKey :: Key -> Loc
locateKey (Key (Loc fp)) = Loc (FP.takeFileName fp)

----------------------------------------------------------------------
-- Build

doBuild :: Config -> How -> Key -> B Digest
doBuild config@Config{logMode,seeD} how = do
  -- TODO: document this flow.
  -- TODO: check for cycles.
  memoDigestByKey $ \sought -> do
    case Map.lookup sought how of
      Nothing -> do
        let Key loc = sought
        Execute (XFileExists loc) >>= \case
          False -> do
            error (printf "'%s' is not source and has no build rule" (show sought))
          True -> do
            digest <- copyIntoCache loc
            pure digest

      Just rule@Rule{depcom} -> do
        let seeV = case logMode of LogVerbose -> True; _ -> False
        when seeV $ BLog (printf "B: Require: %s" (show sought))
        (deps,action@Action{commands}) <- gatherDeps config how depcom

        wdeps <- (WitMap . Map.fromList) <$>
          parallel [ do digest <- doBuild config how dep; pure (locateKey dep,digest)
                   | dep <- deps
                   ]

        let witKey = WitnessKey { commands, wdeps }
        let wkd = digestWitnessKey witKey

        -- If the witness trace already exists, use it.
        when seeD $ Execute $ XLog (printf "L: looking for witness: %s" (show wkd))
        verifyWitness sought wkd >>= \case
          Just digest -> pure digest
          Nothing -> do
            -- If not, someone has to run the job...
            when seeD $ Execute $ XLog (printf "L: no witness; job must be run: %s" (show wkd))
            tryGainingLock seeD wkd $ \case
              True -> do
                -- We got the lock, check again for the trace...
                verifyWitness sought wkd >>= \case
                  Just digest -> pure digest
                  Nothing -> do
                    -- We have the lock and there is still no trace, so we run the job....
                    wtargets <- runJobAndSaveWitness config action wkd witKey wdeps rule
                    let digest = lookWitMap (locateKey sought) wtargets
                    pure digest
              False -> do
                when seeD $ Execute $ XLog (printf "L: running elsewhere: %s" (show wkd))
                -- Another process beat us to the lock; it's running the job.
                awaitLockYielding config wkd
                -- Now it will have finished.
                verifyWitness sought wkd >>= \case
                  Just digest -> pure digest
                  Nothing ->
                    -- This may also occur when the the build does not fail
                    -- We are too eager in expecting the witness trace to appear
                    -- TODO: perhaps we should spin for it?
                    error "other process encountered build failure"

awaitLockYielding :: Config -> WitKeyDigest -> B ()
awaitLockYielding Config{seeD} wkd = loop 0
  where
    loop :: Int -> B ()
    loop i = do
      BYield
      tryGainingLock False wkd $ \case
        False -> loop (i+1)
        True -> do
          when seeD $ Execute $ XLog (printf "L: spun (%d) awaiting lock %s" i (show wkd))
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
      res <- f True
      -- Critical section finished; release the lock (unlink and unlok).
      Execute $ do
        XUnLink (Loc lockPath)
        XIO (unlockFile lock)
        when debug $ XLog $ printf "L: unlocked %s" (show wkd)
        pure res
    Nothing ->
      -- We lost the race; another process has the lock.
      f False

runJobAndSaveWitness :: Config -> Action -> WitKeyDigest -> WitnessKey -> WitMap -> Rule -> B WitMap
runJobAndSaveWitness config action wkd witKey wdeps rule = do
  wtargets <- buildWithRule config action wdeps rule
  let val = WitnessValue { wtargets }
  let wit = Witness { key = witKey, val }
  saveWitness wkd wit
  pure wtargets



memoDigestByKey :: (Key -> B Digest) -> Key -> B Digest
memoDigestByKey f sought = do
  BGetKey sought >>= \case
    Just digest -> pure digest
    Nothing -> do
      digest <- f sought
      BSetKey sought digest
      pure digest


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
  digest <- doBuild config how key
  file <- cacheFile digest
  Execute (XReadFile file)

existsKey :: How -> Key -> B Bool
existsKey how key =
  if Map.member key how
  then pure True
  else do
    let Key loc = key
    Execute (XFileExists loc)


buildWithRule :: Config -> Action -> WitMap -> Rule -> B WitMap
buildWithRule Config{keepSandBoxes} action depWit rule = do
  sandbox <- BNewSandbox
  let Rule{rulename} = rule
  Execute (XMakeDir sandbox)
  setupInputs sandbox depWit
  BRunActionInDir sandbox action >>= \case
    False ->
      error (printf "action failed for rule '%s'" rulename)
    True -> do
      targetWit <- cacheOutputs sandbox rule
      when (not keepSandBoxes) $ Execute (XRemoveDirRecursive sandbox)
      pure targetWit

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
            error (printf "rule '%s' failed to produced declared target '%s'" rulename (show target))
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

data Witness = Witness { key :: WitnessKey, val :: WitnessValue }

data WitnessValue = WitnessValue { wtargets :: WitMap }

-- TODO: target set in witness key? i.e. forcing rerun when add or remove targets
data WitnessKey = WitnessKey { commands :: [String], wdeps :: WitMap } deriving Show

-- TODO: perhaps filemode should be included in the target WitMap
data WitMap = WitMap (Map Loc Digest) deriving Show

-- message digest of a witness key; computer by internal MD5 code
data WitKeyDigest = WitKeyDigest String

instance Show WitKeyDigest where show (WitKeyDigest str) = str

lookWitMap :: Loc -> WitMap -> Digest
lookWitMap loc (WitMap m) = maybe err id $ Map.lookup loc m
  where err = error "lookWitMap"

digestWitnessKey :: WitnessKey -> WitKeyDigest
digestWitnessKey wk = WitKeyDigest (MD5.md5s (MD5.Str (show wk)))

verifyWitness :: Key -> WitKeyDigest -> B (Maybe Digest)
verifyWitness sought wkd = do
  lookupWitness wkd >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      let Witness{val} = wit
      let WitnessValue{wtargets} = val
      let WitMap m = wtargets
      ok <- all id <$> sequence [ existsCacheFile digest | (_,digest) <- Map.toList m ]
      if not ok then pure Nothing else do
        -- The following lookup can fail if the sought-key was not recorded.
        -- i.e. we have added a target; but the actions/deps are otherwise unchanged
        -- the user action will have to be rerun.
        pure $ Map.lookup (locateKey sought) m

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
          --XLog (printf "importWitness failed: [%s]" contents)
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
-- export/import Witness data in fixed format using flatter type

importWitness :: String -> Maybe Witness
importWitness s = fromQ <$> readMaybe s

exportWitness :: Witness -> String
exportWitness = show . toQ

data QWitness
  = WIT { command :: String
        , deps :: QWitMap
        , targets :: QWitMap
        }
  | TRACE { commands :: [String]
          , deps :: QWitMap
          , targets :: QWitMap
          }

  deriving (Show,Read)

type QWitMap = [(FilePath,QDigest)]
type QDigest = String

toQ :: Witness -> QWitness
toQ wit = do
  let Witness{key,val} = wit
  let WitnessKey{commands,wdeps} = key
  let WitnessValue{wtargets} = val
  let fromStore (WitMap m) = [ (fp,digest) | (Loc fp,Digest digest) <- Map.toList m ]
  TRACE { commands, deps = fromStore wdeps, targets = fromStore wtargets }

fromQ :: QWitness -> Witness
fromQ = \case
  WIT{command,deps,targets} -> do
    let toStore xs = WitMap (Map.fromList [ (Loc fp,Digest digest) | (fp,digest) <- xs ])
    let key = WitnessKey{commands=[command], wdeps = toStore deps}
    let val = WitnessValue{wtargets = toStore targets}
    Witness{key,val}
  TRACE{commands,deps,targets} -> do
    let toStore xs = WitMap (Map.fromList [ (Loc fp,Digest digest) | (fp,digest) <- xs ])
    let key = WitnessKey{commands, wdeps = toStore deps}
    let val = WitnessValue{wtargets = toStore targets}
    Witness{key,val}

----------------------------------------------------------------------
-- B: build monad

parallel :: [B a] -> B [a]
parallel = \case
  [] -> pure []
  [p] -> (\x->[x]) <$> p
  p:ps -> (\(x,xs) -> x:xs) <$> BPar p (parallel ps)

instance Functor B where fmap = liftM
instance Applicative B where pure = BRet; (<*>) = ap
instance Monad B where (>>=) = BBind

data B a where -- TODO: BFail?
  BRet :: a -> B a
  BBind :: B a -> (a -> B b) -> B b
  BLog :: String -> B ()
  BCacheDir :: B Loc
  BNewSandbox :: B Loc
  BRunActionInDir :: Loc -> Action -> B Bool
  Execute :: X a -> B a
  BGetKey :: Key -> B (Maybe Digest)
  BSetKey :: Key -> Digest -> B ()
  BPar :: B a -> B b -> B (a,b)
  BYield :: B ()

runB :: Loc -> Config -> B () -> IO ()
runB cacheDir config@Config{keepSandBoxes,logMode,jnum} build0 = do
  nCopies jnum $ do
    runX config $ do
      loop build bstate0 k0
  where
    build = do
      let see = case logMode of LogQuiet -> False; _ -> True
      Execute $ do
        myPid <- XIO getCurrentPid
        when (see && keepSandBoxes) $ do
          XLog (printf "sandboxes created in: %s" (show (sandboxParent myPid)))
      initDirs
      build0

    -- TODO: We should cleanup our sandbox dir on abort.
    -- Currently it gets left if we C-c jenga
    -- Or if any build command fails; since that aborts jenga (TODO: fix that!)
    -- This means that even the cram tests leave .jbox droppings around.
    -- Not the end of the day, but we could be cleaner.

    sandboxParent pid = Loc (printf "/tmp/.jbox/%s" (show pid))

    k0 :: BState -> () -> X ()
    k0 BState{runCounter,active,blocked} () = do
      myPid <- XIO getCurrentPid
      when (not keepSandBoxes) $ XRemoveDirRecursive (sandboxParent myPid)
      when ((length active + length blocked) /= 0) $ error "runB: unexpected left over jobs"
      let see = case logMode of LogQuiet -> False; _ -> True
      let i = runCounter
      when (see && i>0) $ do
        XLog (printf "ran %s" (pluralize i "action"))
      pure ()

    loop :: B a -> BState -> (BState -> a -> X ()) -> X ()
    loop m0 s k = case m0 of
      BRet a -> k s a
      BBind m f -> loop m s $ \s a -> loop (f a) s k
      BLog mes -> do XLog mes; k s ()
      BCacheDir -> k s cacheDir
      BNewSandbox -> do
        myPid <- XIO getCurrentPid
        let BState{sandboxCounter=i} = s
        k s { sandboxCounter = 1 + i } (sandboxParent myPid </> show i)
      BRunActionInDir sandbox action@Action{hidden} -> do
        bool <- XRunActionInDir sandbox action
        k s { runCounter = runCounter s + (if hidden then 0 else 1) } bool
      Execute x -> do a <- x; k s a
      BGetKey key -> do
        let BState{memo} = s
        case Map.lookup key memo of
          Nothing -> k s Nothing
          Just digest -> k s (Just digest)
      BSetKey key digest -> do
        let BState{memo} = s
        k s { memo = Map.insert key digest memo } ()

      BPar a b -> do
        -- continutation k is called only when both sides have completed
        varA <- XIO (newIORef Nothing)
        varB <- XIO (newIORef Nothing)
        let
          kA s a = do
            XIO (readIORef varB) >>= \case
              Nothing -> do XIO (writeIORef varA (Just a)); next s
              Just b -> do k s (a,b)
        let
          kB s b = do
            XIO (readIORef varA) >>= \case
              Nothing -> do XIO (writeIORef varB (Just b)); next s
              Just a -> do k s (a,b)

        loop a s { active = BJob b kB : active s } kA

      BYield -> do
        let BState{blocked} = s
        let me = BJob (pure ()) k
        next s { blocked = me : blocked }

    next :: BState -> X ()
    next s@BState{active,blocked} = do
      case active of
        j:active -> do
          resume j s { active }
        [] -> case blocked of
          [] -> error "next: no more jobs"
          [onlyJob] -> do -- TODO: special code to avoid spinning?
            next s { active = [onlyJob], blocked = [] }
          blocked -> do
            next s { active = reverse blocked, blocked = [] }

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

data BState = BState
  { sandboxCounter :: Int
  , runCounter :: Int -- less that sandboxCounter because of hidden actions
  , memo :: Map Key Digest
  , active :: [BJob]
  , blocked :: [BJob]
  }

bstate0 :: BState
bstate0 = BState
  { sandboxCounter = 0
  , runCounter = 0
  , memo = Map.empty
  , active = []
  , blocked = []
  }

data BJob where
  BJob :: B a -> (BState -> a -> X ()) -> BJob

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

  XRunActionInDir :: Loc -> Action -> X Bool
  XDigest :: Loc -> X Digest

  XMakeDir :: Loc -> X ()
  XGlob :: Loc -> X [Loc]
  XFileExists :: Loc -> X Bool
  XIsdirectory :: Loc -> X Bool
  XCopyFile :: Loc -> Loc -> X ()
  XTransferFileMode :: Loc -> Loc -> X ()
  XMakeReadOnly :: Loc -> X ()
  XReadFile :: Loc -> X String
  XWriteFile :: String -> Loc -> X ()
  XTryHardLink :: Loc -> Loc -> X (Maybe String)
  XUnLink :: Loc -> X ()
  XRemoveDirRecursive :: Loc -> X ()

runX :: Config -> X a -> IO a
runX config@Config{logMode,seeX,seeI,seeD} = loop
  where

    log mes = maybePrefixPid config mes >>= putOut

    seeA = case logMode of LogQuiet -> False; _ -> True
    logA,logX,logI,logD :: String -> IO ()
    logA mes = when seeA $ log (printf "A: %s" mes)
    logX mes = when seeX $ log (printf "X: %s" mes)
    logI mes = when seeI $ log (printf "I: %s" mes)
    logD mes = when seeD $ log (printf "L: %s" mes) -- locking debug

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XIO io -> io
      XLog mes -> log mes
      XLogErr mes -> maybePrefixPid config mes >>= putErr

      -- sandboxed execution of user's action; for now always a bash command
      XRunActionInDir (Loc dir) Action{hidden,commands} -> all id <$> do
        forM commands $ \command -> do

          let showHidden = False -- command line flag?
          if showHidden
            then logA $ printf "%s%s" command (if hidden then " (hidden)" else "")
            else when (not hidden) $ logA $ command

          (exitCode,stdout,stderr) <-
            withCurrentDirectory dir (readCreateProcessWithExitCode (shell command) "")
          -- TODO: better management & report of stdout/stderr
          putStr stdout
          putStr stderr
          let ok = case exitCode of ExitSuccess -> True; ExitFailure{} -> False
          pure ok

      -- other commands with shell out to external process
      XDigest (Loc fp) -> do
        let command = printf "md5sum %s" fp
        logX command
        output <- readCreateProcess (shell command) ""
        let str = case (splitOn " " output) of [] -> error "XDigest/split"; x:_ -> x
        pure (Digest str)

      -- internal file system access (log approx equivalent external command)
      XMakeDir (Loc fp) -> do
        logI $ printf "mkdir -p %s" fp
        createDirectoryIfMissing True fp
      XGlob (Loc fp) -> do
        logI $ printf "ls %s" fp
        xs <- listDirectory fp
        pure [ Loc fp </> x | x <- xs ]
      XFileExists (Loc fp) -> do
        logI $ printf "test -e %s" fp
        fileExist fp
      XIsdirectory (Loc fp) -> do
        logI $ printf "test -d %s" fp
        fileExist fp >>= \case
          False -> pure False
          True -> do
            status <- getFileStatus fp
            pure (isDirectory status)
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
        logD $ printf "cat> %s" dest
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

nCopies :: Int -> IO () -> IO ()
nCopies n io =
  if n < 1 then error "nCopies<1" else do
    sequence_ $ replicate (n-1) $ do _ <- forkProcess io; pure ()
    io

maybePrefixPid :: Config -> String -> IO String
maybePrefixPid Config{seePid} mes = do
  case seePid of
    False -> pure mes
    True -> do
      myPid <- getCurrentPid
      pure $ printf "[%s] %s" (show myPid) mes

putErr :: String -> IO ()
putErr s = do
  hPutStrLn stderr s
  hFlush stderr
  pure ()

putOut :: String -> IO ()
putOut s = do
  hPutStrLn stdout s
  hFlush stdout
  pure ()
