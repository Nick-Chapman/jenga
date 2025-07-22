module Interface
  ( G(..)       -- The Generation monad; used to define rules and artifacts.
  , Rule(..)    -- A Rule with dynamic dependencies.
  , Action(..)  -- A user action which is run when a rule triggers.
  , D(..)       -- The Dependency monad.
  , Key(..)     -- An identifier for targets and dependencies.
  , Loc(..)     -- A relative file-path location.
  ) where

import Control.Monad (ap,liftM)

instance Functor G where fmap = liftM
instance Applicative G where pure = GRet; (<*>) = ap
instance Monad G where (>>=) = GBind

data G a where
  GRet :: a -> G a
  GBind :: G a -> (a -> G b) -> G b
  GLog :: String -> G ()
  GFail :: String -> G a
  GRule :: Rule -> G ()
  GGlob :: Loc -> G [Loc]
  GIsDirectory :: Loc -> G Bool
  GExists :: Loc -> G Bool
  GReadKey :: Key -> G String
  -- TODO: construct for parallel generation

data Rule = Rule
  { rulename :: String
  , dir :: Loc
  , hidden :: Bool
  , targets :: [Key]
  , depcom :: D Action
  }

data Action = Action
  { hidden :: Bool
  , commands :: [String]
  }

instance Functor D where fmap = liftM
instance Applicative D where pure = DRet; (<*>) = ap
instance Monad D where (>>=) = DBind

data D a where
  DRet :: a -> D a
  DBind :: D a -> (a -> D b) -> D b
  DLog :: String -> D ()
  DNeed :: Key -> D ()
  DReadKey :: Key -> D String
  DExistsKey :: Key -> D Bool

data Key = Key Loc deriving (Eq,Ord)

data Loc = Loc FilePath deriving (Eq,Ord)

instance Show Key where show (Key loc) = show loc
instance Show Loc where show (Loc fp) = fp
