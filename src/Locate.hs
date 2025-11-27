
module Locate
  ( Loc -- file-system location
  , Dir -- location which is a directory
  , Tag -- basename of a location; used for engine witnesses

  -- leaving the nicely typed world
  , pathOfLoc
  , pathOfDir
  , stringOfTag

  -- constructors
  , makeAbsoluteDir, makeTag
  , (</>)

  , takeDir
  , takeBase

  , locOfDir
  , insistLocIsDir
  ) where

import System.FilePath qualified as FP
import System.Path.NameManip (guess_dotdot)

-- types... invariant: Loc and Dir *always* start with a '/'

newtype Loc = LocX FilePath deriving (Eq,Ord)
newtype Dir = DirX FilePath deriving Eq
newtype Tag = TagX String deriving (Eq,Ord)

instance Show Tag where show (TagX s) = s

-- leaving the type world...

pathOfLoc :: Loc -> FilePath
pathOfLoc (LocX fp) = fp

pathOfDir :: Dir -> FilePath
pathOfDir (DirX fp) = fp

stringOfTag :: Tag -> String
stringOfTag (TagX s) = s

-- basic constructors...

makeAbsoluteDir :: FilePath -> Dir
makeAbsoluteDir fp =
  case fp of
    '/':_ -> DirX fp
    _ -> error (show ("makeAbsoluteDir",fp))

makeTag :: String -> Tag
makeTag s = TagX s -- TODO: check contains no /s

-- moving between the types...

takeDir :: Loc -> Dir
takeDir (LocX fp) = makeAbsoluteDir (FP.takeDirectory fp)

takeBase :: Loc -> Tag
takeBase (LocX fp) = makeTag (FP.takeFileName fp)

(</>) :: Dir -> String -> Loc
(</>) (DirX dir) path =
  case path of
    '/':_ -> LocX path -- TODO: do we get this anyway from FP.</>
    rel -> do
      LocX (FP.normalise $ removeDotdotIsPossible (dir FP.</> rel))
      where
        removeDotdotIsPossible :: String -> String
        removeDotdotIsPossible s =
          case guess_dotdot s of Just s -> s; Nothing -> s

locOfDir :: Dir -> Loc -- upcast; TODO: shame
locOfDir (DirX fp) = LocX fp

insistLocIsDir :: Loc -> Dir
insistLocIsDir loc = makeAbsoluteDir (pathOfLoc loc)
