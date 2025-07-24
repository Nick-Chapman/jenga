module StdBuildUtils
  ( mkKey
  , baseKey
  , dirKey
  , (</>)
  , dirLoc
  ) where

import Interface (Key(..),Loc(..))
import System.FilePath qualified as FP

mkKey :: Loc -> String -> Key
mkKey (Loc fp) suffix = Key (Loc (fp++suffix))

baseKey :: Key -> String
baseKey (Key (Loc fp)) = FP.takeFileName fp

dirKey :: Key -> Loc
dirKey (Key loc) = dirLoc loc

dirLoc :: Loc -> Loc
dirLoc (Loc fp) = Loc (FP.takeDirectory fp)

(</>) :: Loc -> String -> Loc
(</>) (Loc dir) filename0 =
  Loc (FP.normalise (dir FP.</> filename))
  where
    filename =
      case filename0 of
        '/':afterLeadingSlash -> afterLeadingSlash
        rel -> rel
