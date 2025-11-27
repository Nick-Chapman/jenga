-- | Syntax and elaboration for build.jenga files
module Syntax (elaborate) where

import Control.Monad (when)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Printf (printf)

import CommandLine (Config(..))
import Interface (G(..),Rule(..),Action(..),D(..),Key(..),Target(..),Artifact(..),What(..))
import Locate (Loc,Dir,(</>),takeDir,takeBase,locOfDir,stringOfTag,pathOfDir,pathOfLoc)
import Par4 (Position(..),Par,parse,position,skip,alts,many,some,sat,lit,key)

elaborate :: Config -> Key -> G ()
elaborate Config{homeDir,withPromotion} dotJengaFile0 = do
  when withPromotion $ promoteRule
  allFilesRule
  elabRuleFile dotJengaFile0
  where
    dir = dirKey dotJengaFile0

    dirKey :: Key -> Dir
    dirKey (Key loc) = takeDir loc

    pathOfKey :: Key -> FilePath
    pathOfKey (Key loc) = pathOfLoc loc

    elabRuleFile :: Key -> G ()
    elabRuleFile dotJengaFile  = do
      s <- GReadKey dotJengaFile
      case Par4.parse (pathOfKey dotJengaFile) gram s of
        Left parseError -> GFail parseError
        Right clauses -> mapM_ elabClause clauses

      where
        elabClause :: Clause -> G ()
        elabClause = \case
          ClauseTrip x -> elabTrip x
          ClauseInclude filename -> elabRuleFile (makeKey filename)

        elabTrip :: Trip -> G ()
        elabTrip Trip{pos=Position{line},ruleTarget,deps,commands=commands0} = do
          let rulename = printf "%s:%d" (pathOfKey dotJengaFile) line
          let
            artNames =
              case ruleTarget of
                MArtifacts xs -> [ name | (_,name) <- xs ]
                MPhony{} -> []
          let commands = map expandSpecial commands0
          GRule $ Rule
            { rulename
            , dir
            , hidden = False
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
            where
              dollarAtReplacement =
                case ruleTarget of
                  MArtifacts xs ->
                    intercalate " " [ stringOfTag (takeBase (dir </> name))
                                    | (_,name) <- xs ]
                  MPhony name -> name

              -- very simplistic support for $$, $@, $^ and $<
              dollarHatReplacement =
                intercalate " " [ stringOfTag (takeBase (dir </> name))
                                | DepPlain name <- deps ]
              dollarLeftReplacement =
                intercalate " " (take 1 [ stringOfTag (takeBase (dir </> name))
                                        | DepPlain name <- deps])
              expandSpecial :: String -> String
              expandSpecial = loop
                where
                  loop = \case
                    [] -> []
                    '$':'$':rest -> '$' : loop rest
                    '$':'@':rest -> dollarAtReplacement ++ loop rest
                    '$':'^':rest -> dollarHatReplacement ++ loop rest
                    '$':'<':rest -> dollarLeftReplacement ++ loop rest
                    x:xs -> x : loop xs

    bash :: [String] -> Action
    bash commands = Action { hidden = False, commands }

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

    -- hidden rule so user-rules can access the list of file names
    allFilesName = "all.files"
    allFilesRule =  do
      allFiles <- map Key <$> glob dir
      GRule (Rule { rulename = printf "glob-%s" (pathOfDir dir)
                  , dir
                  , hidden = True
                  , target = Artifacts [ Artifact { materialize = False
                                                  , key = makeKey allFilesName } ]
                  , depcom = pure (Action
                                    { hidden = True
                                    , commands = [printf "echo -n '%s' > %s"
                                                  (unlines (map baseKey allFiles))
                                                  allFilesName]
                                    })})

        where
          baseKey :: Key -> String
          baseKey (Key loc) = stringOfTag (takeBase loc)

    promoteName = ".promote"
    promoteRule =  do
      GRule (Rule { rulename = printf ".promote-%s" (pathOfDir dir)
                  , dir
                  , hidden = True
                  , target = Artifacts [ Artifact { materialize = False
                                                  , key = makeKey promoteName } ]
                  , depcom = pure (Action
                                    { hidden = True
                                    , commands = [printf "touch %s"
                                                  promoteName]
                                    })})


glob :: Dir -> G [Loc]
glob dir = do
  GWhat (locOfDir dir) >>= \case
    Directory entries -> pure [ dir </> e | e <- entries ]
    _what -> GFail (printf "glob: expected %s to be a directory" (pathOfDir dir))

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
  { pos :: Position
  , ruleTarget :: MTarget
  , deps :: [Dep]
  , commands :: [String]
  }

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
      (filter (\case "" -> False; _ -> True)) <$> many indentedCommand

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

    specialChar = (`elem` " :#()\n!*")

    singleCommandLine = do
      trimTrailingSpace <$> many actionChar

    actionChar = sat $ \case -- anything upto a NL (leaving #-comments for bash)
      '\n' -> False
      _ -> True

    trimTrailingSpace = reverse . dropWhile (==' ') . reverse

    space = lit ' '
    nl = lit '\n'

    commentToEol = do
      lit '#'
      skip notNL
      lit '\n'

    notNL = do
      _ <- sat (\case '\n' -> False; _ -> True)
      pure ()
