module StdBuildUtils
  ( mkKey
  , baseKey
  , dirKey
  , (</>)
  , dirLoc
  ) where

import Interface (Key(..),Loc(..))
import System.FilePath qualified as FP
import System.Path.NameManip (guess_dotdot)

mkKey :: Loc -> String -> Key
mkKey (Loc fp) suffix = Key (Loc (fp++suffix))

baseKey :: Key -> String
baseKey (Key (Loc fp)) = FP.takeFileName fp

dirKey :: Key -> Loc
dirKey (Key loc) = dirLoc loc

dirLoc :: Loc -> Loc
dirLoc (Loc fp) = Loc (FP.takeDirectory fp)

(</>) :: Loc -> String -> Loc
(</>) (Loc dir) path = -- TODO: support tilda (~) expansion
  case path of
    '/':_ -> Loc path
    rel ->
      Loc (FP.normalise $ removeDotdotIsPossible (dir FP.</> rel))

removeDotdotIsPossible :: String -> String
removeDotdotIsPossible s = case guess_dotdot s of Just s -> s; Nothing -> s
