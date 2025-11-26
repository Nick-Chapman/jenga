
module Locate
  ( Loc -- file-system location
  , Dir -- location which is a directory
  , Tag -- basename of a location; used for engine witnesses

  -- leaving the nicely typed world
  , pathOfLoc
  , pathOfDir
  , stringOfTag

  -- constructors (should check invariant)
  , makeLoc, makeDir, makeTag

  , takeDir
  , takeBase
  , (</>)

  , locOfDir
  , subDir
  , insistLocIsDir
  ) where

import System.FilePath qualified as FP
import System.Path.NameManip (guess_dotdot)

-- types...

data Loc = LocX FilePath deriving (Eq,Ord)
instance Show Loc where show (LocX fp) = fp

data Dir = DirX FilePath deriving Eq
instance Show Dir where show (DirX fp) = fp

data Tag = TagX String deriving (Eq,Ord)
instance Show Tag where show (TagX s) = s

-- leaving the type world...

pathOfLoc :: Loc -> FilePath
pathOfLoc (LocX fp) = fp

pathOfDir :: Dir -> FilePath
pathOfDir (DirX fp) = fp

stringOfTag :: Tag -> String
stringOfTag (TagX s) = s

-- basic constructors; TODO: do some checking
makeLoc :: FilePath -> Loc
makeLoc fp = LocX fp

makeDir :: FilePath -> Dir
makeDir fp = DirX fp

makeTag :: String -> Tag
makeTag s = TagX s

-- moving between the types...

takeDir :: Loc -> Dir
takeDir (LocX fp) = makeDir (FP.takeDirectory fp)

takeBase :: Loc -> Tag
takeBase (LocX fp) = makeTag (FP.takeFileName fp)

(</>) :: Dir -> String -> Loc
(</>) (DirX dir) path =
  case path of
    '/':_ -> makeLoc path
    rel ->
      makeLoc (FP.normalise $ removeDotdotIsPossible (dir FP.</> rel))
      where
        removeDotdotIsPossible :: String -> String
        removeDotdotIsPossible s =
          case guess_dotdot s of Just s -> s; Nothing -> s

locOfDir :: Dir -> Loc -- upcast; TODO: shame
locOfDir (DirX fp) = LocX fp

subDir :: Dir -> String -> Dir
subDir dir e = insistLocIsDir (dir </> e)

insistLocIsDir :: Loc -> Dir
insistLocIsDir loc = makeDir (show loc)

