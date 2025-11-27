
module Locate
  ( Loc -- file-system location
  , Dir -- location which is a directory
  , Tag -- basename of a location; used for engine witnesses

  -- leaving the nicely typed world
  , pathOfLoc
  , pathOfDir
  , stringOfTag

  -- constructors
  , makeLoc, makeDir, makeTag

  , takeDir
  , takeBase
  , (</>)

  , locOfDir
  , insistLocIsDir
  ) where

import System.FilePath qualified as FP
import System.Path.NameManip (guess_dotdot)

-- types...

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

makeLoc :: FilePath -> Loc -- TODO: think we only use this for dirs. so remove?
makeLoc fp =
  case fp of
    '/':_ -> LocX fp -- absolute; ok
    _ -> error (show ("makeLoc/not-absolute",fp))

makeDir :: String -> FilePath -> Dir
makeDir who fp = -- TODO: remove who?
  case fp of
    '/':_ -> DirX fp -- absolute; ok
    _ -> error (show ("makeDir/not-absolute",who,fp))

makeTag :: String -> Tag
makeTag s = TagX s

-- moving between the types...

takeDir :: Loc -> Dir
takeDir (LocX fp) = makeDir "[takeDir]" (FP.takeDirectory fp)

takeBase :: Loc -> Tag
takeBase (LocX fp) = makeTag (FP.takeFileName fp)

(</>) :: Dir -> String -> Loc
(</>) (DirX dir) path =
  case path of
    '/':_ -> makeLoc path -- TODO: when does this happen?
    rel -> do
      LocX (FP.normalise $ removeDotdotIsPossible (dir FP.</> rel))
      where
        removeDotdotIsPossible :: String -> String
        removeDotdotIsPossible s =
          case guess_dotdot s of Just s -> s; Nothing -> s

locOfDir :: Dir -> Loc -- upcast; TODO: shame
locOfDir (DirX fp) = LocX fp

insistLocIsDir :: Loc -> Dir
insistLocIsDir loc = makeDir "[insistLocIsDir]" (pathOfLoc loc)
