
module Interface
  ( G(..)       -- The Generation monad; used to define rules and artifacts.
  , Rule(..)    -- A Rule with dynamic dependencies.
  , Action(..)  -- A user action which is run when a rule triggers.
  , D(..)       -- The Dependency monad.
  , Target(..)  -- list of artifacts or an action name
  , Artifact(..)-- artifact name + materialization control
  , What(..)    -- what kind of file-system object is at a given location
  , Key(..)     -- An identifier for artifacts and dependencies.
  ) where

import Control.Monad (ap,liftM)
import Locate (Loc,Dir)

instance Functor G where fmap = liftM
instance Applicative G where pure = GRet; (<*>) = ap
instance Monad G where (>>=) = GBind

data G a where
  GRet :: a -> G a
  GBind :: G a -> (a -> G b) -> G b
  GLog :: String -> G ()
  GFail :: String -> G a
  GRule :: Rule -> G ()
  GWhat :: Loc -> G What
  GReadKey :: Key -> G String
  GExistsKey :: Key -> G Bool
  GPar :: G a -> G b -> G (a,b)

data What = Missing | File | Link | Directory { entries :: [String] }

data Rule = Rule
  { rulename :: String
  , dir :: Dir
  , hidden :: Bool -- TODO: remove this when all.files rule is reworked
  , target :: Target
  , depcom :: D Action
  }

data Target = Artifacts [Artifact] | Phony String

data Artifact = Artifact
  { materialize :: Bool
  , key :: Key
  }

data Action = Action
  { hidden :: Bool -- TODO: remove?
  , commands :: [String] -- TODO: can we revert back to Action == Single Command?
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
