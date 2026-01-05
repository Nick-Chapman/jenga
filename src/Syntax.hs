-- | Syntax and elaboration for build.jenga files
module Syntax (elaborate) where

import Data.List (intercalate,sort)
import Data.List.Split (splitOn)
import Data.Text qualified as Text (pack)
import Text.Printf (printf)

import CommandLine (Config(..))
import Interface (G(..),Rule(..),Action(..),D(..),Key(..),Target(..),Artifact(..),What(..))
import Locate (Loc,Dir,Tag,(</>),takeDir,takeBase,locOfDir,stringOfTag,insistLocIsDir)
import Par4 (Pos(..),Par,parse,position,skip,alts,many,some,sat,lit,key)

elaborate :: (Key -> String) -> Config -> Key -> G ()
elaborate ppKey config@Config{homeDir} dotJengaFile0 = do -- TODO: pass ppKey in config?
  elabRuleFile dotJengaFile0
  where
    dir = dirKey dotJengaFile0

    dirKey :: Key -> Dir
    dirKey (Key loc) = takeDir loc

    elabRuleFile :: Key -> G ()
    elabRuleFile dotJengaFile  = do
      t <- Text.pack <$> GReadKey dotJengaFile
      let filename = ppKey dotJengaFile
      case Par4.parse gram t of
        Left parseError ->
          GFail (printf "%s: %s" filename parseError)
        Right clauses ->
          mapM_ elabClause clauses

      where
        elabClause :: Clause -> G ()
        elabClause = \case
          ClauseTrip x -> elabTrip x
          ClauseInclude filename -> elabRuleFile (makeKey filename)

        elabTrip :: Trip -> G ()
        elabTrip Trip{ruleTarget,deps,commands=commands0} = do
          let
            artNames =
              case ruleTarget of
                MArtifacts xs -> [ name | (_,name) <- xs ]
                MPhony{} -> []
          commands <- mapM (expandChunks ppKey config dir ruleTarget deps) commands0
          GRule $ Rule
            { dir
            , target =
              case ruleTarget of
                MArtifacts xs ->
                  Artifacts [ Artifact { materialize = mat, key = makeKey name }
                            | (mat, name) <- xs ]
                MPhony name ->
                  Phony name
            , depcom = do
                sequence_ [ makeDep artNames dep | dep <- deps ]
                pure (bash commands)
            }

    bash :: [String] -> Action
    bash commands = Action { commands }

    makeDep artNames = \case
      DepPlain file -> DNeed (makeKey file)
      DepScanner file -> do
        let key = makeKey file
        contents <- DReadKey key
        let deps = filterDepsFor artNames contents
        sequence_ [ DNeed (makeKey dep) | dep <- deps ]
      DepOpt file -> do
        let key = makeKey file
        b <- DExistsKey key
        if b then DNeed key else pure ()

    makeKey :: String -> Key
    makeKey = \case
      '~':'/':str -> Key (homeDir </> str) -- expand tilda
      str -> Key (dir </> str)

expandChunks :: (Key -> String) -> Config -> Dir -> MTarget -> [Dep] -> [ActChunk] -> G String
expandChunks ppKey Config{withPromotion} dir ruleTarget deps chunks =
  (trimTrailingSpace . concat) <$> mapM expand1 chunks
  where
    expand1 = \case
      AC_String s -> pure s
      AC_DollarOut -> pure dollarAtReplacement
      AC_DollarIns -> pure dollarHatReplacement
      AC_DollarIn1 -> pure dollarLeftReplacement
      AC_DollarGlob globSuffix -> do
        allFiles <- glob ppKey (insistLocIsDir (dir </> globSuffix))
        pure (intercalate "\\n" (map stringOfTag allFiles))

      AC_DollarPromote ->
        pure (if withPromotion then "promote" else "")

    trimTrailingSpace = reverse . dropWhile (==' ') . reverse

    dollarAtReplacement =
      case ruleTarget of
        MArtifacts xs ->
          intercalate " " [ stringOfTag (takeBase (dir </> name))
                          | (_,name) <- xs ]
        MPhony name -> name

    dollarHatReplacement =
      intercalate " " [ stringOfTag (takeBase (dir </> name))
                      | DepPlain name <- deps ]

    dollarLeftReplacement =
      intercalate " " (take 1 [ stringOfTag (takeBase (dir </> name))
                              | DepPlain name <- deps])

glob :: (Key -> String) -> Dir -> G [Tag]
glob ppKey dir = do
  GWhat (locOfDir dir) >>= \case
    Directory entries -> do
      xs <- sequence
        [ do exists <- GExistsKey (Key loc); pure (loc,exists)
        | e <- entries, let loc = dir </> e
        ]
      ys <- sequence
        [ do isDir <- isDirectory loc; pure (loc,isDir)
        | (loc,exists) <- xs, exists
        ]
      pure (sort [ takeBase loc | (loc,isDir) <- ys, not isDir ])
    _what -> do
      GFail $ printf "glob: expected '%s' to be a directory" (ppKey (Key (locOfDir dir)))

isDirectory :: Loc -> G Bool
isDirectory loc = do
  GWhat loc >>= \case
    Directory{} -> pure True
    _ -> pure False

filterDepsFor :: [String] -> String -> [String]
filterDepsFor artNames contents = do
  let
    parseDepsLine :: String -> [String]
    parseDepsLine line =
      case splitOn ":" line of
        -- If a 'deps' line contains a colon,
        -- we regard names on the right as the list of deps,
        -- but only take them if we target a name listed on the left.
        [left,right] -> do
          if any (`elem` artNames) (words left) then words right else []
        _ -> do
          -- No colon: we take all the deps
          words line

  [ dep | line <- lines contents, dep <- parseDepsLine line ]


data Clause = ClauseTrip Trip | ClauseInclude String

data Trip = Trip
  { pos :: Pos -- TODO: unused?
  , ruleTarget :: MTarget
  , deps :: [Dep]
  , commands :: [[ActChunk]]
  }

data ActChunk
  = AC_String String
  | AC_DollarOut -- TODO: $out AND $outs
  | AC_DollarIns
  | AC_DollarIn1
  | AC_DollarGlob String
  | AC_DollarPromote

data MTarget
  = MArtifacts [(Bool,String)] -- Bool indicates user materizalization (!)
  | MPhony String

data Dep
  = DepPlain String     -- key
  | DepScanner String   -- @key
  | DepOpt String       -- ?key

-- grammar for traditional "make-style" triples, spread over two lines.
-- Extended to allow scanner deps of the form "@file"

gram :: Par [Clause]
gram = start
  where
    start = do
      skip $ alts [space,nl,commentToEol]
      many clause

    clause = do
      alts [includeClause,ruleClause]

    includeClause = do
      Par4.key "include"
      space
      skip space
      fileName <- identifier
      skip space
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure (ClauseInclude fileName)

    ruleClause = do
      pos <- position
      ruleTarget <- ruleTarget
      colon
      deps <- many dep
      commands <- alts [tradRule,onelineRule]
      skip $ alts [nl,commentToEol]
      pure (ClauseTrip (Trip {pos,ruleTarget,deps,commands}))

    ruleTarget = do
      alts [ MArtifacts <$> some artifactName
           , MPhony <$> actionName ]

    artifactName = do
      mat <- alts [ do lit '!'; pure True, pure False ]
      name <- identifier
      pure (mat,name)

    actionName = do
      lit '*'
      identifier

    -- traditional make syntax
    tradRule = do
      alts [nl,commentToEol]
      many indentedCommand

    indentedCommand = do
      space -- at least one space char to begin the action
      skip space
      command <- singleCommandLine
      alts [nl,commentToEol]
      pure command

    onelineRule = do
      colon
      command <- singleCommandLine
      alts [nl,commentToEol]
      pure [command]

    -- syntax to allow a rule on a single line
    colon = do
      lit ':'
      skip space

    dep = do
      alts [ do lit '@'; x <- identifier; pure (DepScanner x)
           , do lit '?'; x <- identifier; pure (DepOpt x)
           , DepPlain <$> identifier ]

    identifier = do
      res <- some identifierChar
      skip space -- post skip space
      pure res

    identifierChar = sat (not . specialChar)

    specialChar = (`elem` " :#()\n!*'")

    singleCommandLine = do
      many actionChunk

    actionChunk = alts
      [ do Par4.key "$$"; pure (AC_String "$")
      , do Par4.key "$@"; pure AC_DollarOut
      , do Par4.key "$^"; pure AC_DollarIns
      , do Par4.key "$<"; pure AC_DollarIn1
      , do Par4.key "$out"; pure AC_DollarOut
      , do Par4.key "$ins"; pure AC_DollarIns
      , do Par4.key "$in"; pure AC_DollarIn1
      , do Par4.key "$glob:"; dir <- identifier; pure (AC_DollarGlob dir)
      , do Par4.key "$glob"; pure (AC_DollarGlob ".")
      , do Par4.key "$promote"; pure AC_DollarPromote
      , do s <- some actionChar; pure (AC_String s)
      ]

    actionChar = sat $ \case -- anything upto a NL (leaving #-comments for bash)
      '\n' -> False
      '$' -> False
      _ -> True

    space = lit ' '
    nl = lit '\n'

    commentToEol = do
      lit '#'
      skip notNL
      lit '\n'

    notNL = do
      _ <- sat (\case '\n' -> False; _ -> True)
      pure ()
