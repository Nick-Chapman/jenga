module MakeStyle (elaborate) where

import Data.List.Split (splitOn)
import Interface (G(..),Rule(..),Action(..),D(..),Key(..))
import Par4 (Position(..),Par,parse,position,skip,alts,many,some,sat,lit,key)
import StdBuildUtils ((</>),dirKey,baseKey)
import Text.Printf (printf)

elaborate :: Key -> G ()
elaborate config0  = do
  allFilesRule
  elabRuleFile config0
  where
    dir = dirKey config0 -- A rule is w.r.t the directory of the build.jenga config file

    elabRuleFile :: Key -> G ()
    elabRuleFile config  = do
      s <- GReadKey config
      case Par4.parse (show config) gram s of
        Left parseError -> GFail parseError
        Right clauses -> mapM_ elabClause clauses

      where
        elabClause :: Clause -> G ()
        elabClause = \case
          ClauseTrip x -> elabTrip x
          ClauseInclude filename -> elabRuleFile (makeKey filename)

        elabTrip :: Trip -> G ()
        elabTrip Trip{pos=Position{line},targets,deps,commands} = do
          let rulename = printf "%s:%d" (show config) line
          GRule $ Rule
            { rulename
            , dir
            , hidden = False
            , targets = map makeKey targets
            , depcom = do
                sequence_ [ makeDep targets dep | dep <- deps ]
                pure (bash commands)
            }

    bash :: [String] -> Action
    bash commands = Action { hidden = False, commands }

    makeDep targets = \case
      DepPlain file -> DNeed (makeKey file)
      DepScanner file -> do
        let key = makeKey file
        contents <- DReadKey key
        let deps = filterDepsFor targets contents
        sequence_ [ DNeed (makeKey dep) | dep <- deps ]
      DepOpt file -> do
        let key = makeKey file
        b <- DExistsKey key
        if b then DNeed key else pure ()

    makeKey :: String -> Key
    makeKey basename = Key (dir </> basename)

    -- hidden rule so user-rules can access the list of file names
    allFilesName = "all.files"
    allFilesRule =  do
      allFiles <- map Key <$> GGlob dir
      GRule (Rule { rulename = printf "glob-%s" (show dir)
                  , dir
                  , hidden = True
                  , targets = [ makeKey allFilesName ]
                  , depcom = pure (Action
                                    { hidden = True
                                    , commands = [printf "echo -n '%s' > %s"
                                                  (unlines (map baseKey allFiles))
                                                  allFilesName]
                                    })})

filterDepsFor :: [String] -> String -> [String]
filterDepsFor targets contents = do
  let
    parseDepsLine :: String -> [String]
    parseDepsLine line =
      case splitOn ":" line of
        -- If a 'deps' line contains a colon,
        -- we regard names on the right as the list of deps,
        -- but only take them if we target a name listed on the left.
        [left,right] -> do
          if any (`elem` targets) (words left) then words right else []
        _ -> do
          -- No colon: we take all the deps
          words line

  [ dep | line <- lines contents, dep <- parseDepsLine line ]


data Clause = ClauseTrip Trip | ClauseInclude String

data Trip = Trip
  { pos :: Position
  , targets :: [String]
  , deps :: [Dep]
  , commands :: [String]
  }

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
      key "include"
      space
      skip space
      fileName <- identifier
      skip space
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure (ClauseInclude fileName)

    ruleClause = do
      pos <- position
      targets <- some identifier
      colon
      deps <- many dep
      commands <- alts [tradRule,onelineRule]
      skip $ alts [nl,commentToEol]
      pure (ClauseTrip (Trip {pos,targets,deps,commands}))

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

    specialChar = (`elem` " :#()\n")

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
