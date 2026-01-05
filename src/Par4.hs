-- | 4-value Parser Combinators
module Par4 (Par,parse,word,key,int,ws0,ws1,sp,nl,lit,sat,char,alts,opt,skip,separated,terminated,many,some,digit,dot,noError,Pos(..),position) where

import Control.Applicative (Alternative,empty,(<|>),many,some)
import Control.Monad (ap,liftM)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)
import qualified Data.Char as Char

instance Alternative Par where empty = Fail; (<|>) = Alt
instance Applicative Par where pure = Ret; (<*>) = ap
instance Functor Par where fmap = liftM
instance Monad Par where (>>=) = Bind

skip :: Par () -> Par ()
separated :: Par () -> Par a -> Par [a]
terminated :: Par () -> Par a -> Par [a]
opt :: Par a -> Par (Maybe a)
alts :: [Par a] -> Par a
word :: Par String
key :: String -> Par ()
int :: Par Int
ws1 :: Par ()
ws0 :: Par ()
digit :: Par Int
sp :: Par ()
nl :: Par ()
lit :: Char -> Par ()
dot :: Par Char
sat :: (Char -> Bool) -> Par Char
char :: Par Char
noError :: Par a -> Par a
position :: Par Pos

skip p = do _ <- many p; return ()
separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
terminated term p = alts [ pure [], do x <- p; term; xs <- terminated term p; pure (x:xs) ]
opt p = alts [ pure Nothing, fmap Just p ]
alts = foldl Alt Fail
word = some $ sat Char.isAlpha
key cs = NoError (mapM_ lit cs)
int = foldl (\acc d -> 10*acc + d) 0 <$> some digit
ws1 = do sp; ws0
ws0 = do _ <- many sp; return ()
digit = (\c -> Char.ord c - ord0) <$> sat Char.isDigit where ord0 = Char.ord '0'
sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()
dot = sat (/= '\n')
sat = Satisfy
char = sat (const True)
noError = NoError
position = Position

data Par a where
  Position :: Par Pos
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Satisfy :: (Char -> Bool) -> Par Char
  NoError :: Par a -> Par a
  Alt :: Par a -> Par a -> Par a

-- Four continuations:
data K4 a b = K4
  { eps :: a -> Res b                  -- success; *no* input consumed
  , succ :: Pos -> Text -> a -> Res b  -- success; input consumed
  , fail :: Res b                      -- failure; *no* input consumed
  , err :: Pos -> Text -> Res b        -- failure; input consumed (so an error!)
  }

type Res a = Either String a

parse :: Par a -> Text -> Either String a
parse parStart text0 = run pos0 text0 parStart finish
  where
    finish :: K4 x x
    finish =
      K4 { eps = yes pos0 text0
         , succ = yes
         , fail = no pos0 text0
         , err = no
         }
      where
        yes p t a =
          if Text.null t then Right a else
            Left $ printf "unparsed input from %s" (report p t)

        no p t =
          Left $ printf "failed to parse %s" (report p t)

        report :: Pos -> Text -> String
        report p t = item ++ " at " ++ show p
          where
            item = case (Text.uncons t) of Nothing -> "<EOF>"; Just (c,_) -> show c

    run :: Pos -> Text -> Par a -> K4 a b -> Res b
    run p t par k@K4{eps,succ,fail,err} = case par of

      Position -> eps p

      Ret x -> eps x

      Fail -> fail

      Satisfy pred -> do
        case Text.uncons t of
          Nothing -> fail
          Just (c,t) -> if pred c then succ (tickPos p c) t c else fail

      NoError par -> do
        run p t par K4 { eps = eps
                       , succ = succ
                       , fail = fail
                       , err = \_ _ -> fail
                       }

      Alt p1 p2 -> do
        run p t p1 K4{ eps = \a1 ->
                         run p t p2 K4{ eps = \_ -> eps a1 -- left biased
                                      , succ
                                      , fail = eps a1
                                      , err
                                      }
                     , succ
                     , fail = run p t p2 k
                     , err
                     }

      Bind par f -> do
        run p t par K4{ eps = \a -> run p t (f a) k
                      , succ = \p t a ->
                          run p t (f a) K4{ eps = \a -> succ p t a -- consume
                                          , succ
                                          , fail = err p t -- fail->error
                                          , err
                                          }
                      , fail
                      , err
                      }

data Pos = Pos { line :: Int, col :: Int } deriving (Eq,Ord)

instance Show Pos where
  show Pos{line,col} = show line ++ "'" ++ show col

pos0 :: Pos
pos0 = Pos { line = 1, col = 0 }

tickPos :: Pos -> Char -> Pos
tickPos Pos {line,col} = \case
  '\n' -> Pos { line = line + 1, col = 0 }
  _ -> Pos { line, col = col + 1 }
