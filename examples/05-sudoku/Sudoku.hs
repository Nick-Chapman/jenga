module Sudoku (main) where

import Data.Char (ord)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set (Set,union,(\\),singleton)
import qualified Data.Set as Set (toList,fromList)
import qualified Data.Map as Map

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let Config {paths,limit} = parseCommandLine args
  let path = case paths of [] -> error "no path given"; [x] -> x; _ -> error "too many paths given"
  g <- parseGivens <$> readFile path
  print g
  case limit of
    Just max -> do
      let sols = take max (solutions g)
      mapM_ print sols
    Nothing -> do
      let sols = solutions g
      mapM_ print sols
      print (length sols)

data Config = Config { paths :: [FilePath], limit :: Maybe Int }

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    config0 = Config { paths = [], limit = Nothing }
    loop config = \case
      [] -> config
      "-1":xs       -> loop config { limit = Just 1 } xs
      "-2":xs       -> loop config { limit = Just 2 } xs
      "-3":xs       -> loop config { limit = Just 3 } xs
      "-limit":n:xs -> loop config { limit = Just (read n) } xs
      ('-':flag):_  -> error (show ("unknown flag",flag))
      x:xs          -> loop config { paths = paths config ++ [x] } xs

solutions :: Grid -> [Grid]
solutions g = if illformed g then [] else search g
  where
    search :: Grid -> [Grid]
    search g = do
      case infer g of
        Done -> [g]
        Fail -> []
        Infer p d -> search (extendG g (p,d))
        Choice p ds -> concat [ search (extendG g (p,d)) | d <- ds ]


data Res = Done | Fail | Infer Pos Digit | Choice Pos [Digit]

infer :: Grid -> Res
infer g = do
  let m = [ (p, Set.toList (allowed g p)) | p <- allPos , not (filled g p)]
  case [ p | (p,[]) <- m ] of
    _:_ -> Fail
    [] -> do
      let xs = [ (p,d) | (p,[d]) <- m ]
      case xs of
        (p,d):_ -> Infer p d
        [] -> do
          case m of
            [] -> Done
            _:_ -> do
              let xs = [(p,ds,length ds) | (p,ds) <- m ]
              let third (_,_,n) = n
              let xs' = sortBy (comparing third) xs
              let (p,ds,_) = case xs' of [] -> error "infer"; x:_ -> x
              Choice p ds


-- special case for illegally placed givens
illformed :: Grid -> Bool
illformed g = any bad allPos
  where bad p = case lookG g p of Nothing -> False; Just d -> d `elem` disallowed g p

allowed :: Grid -> Pos -> Set Digit
allowed g p = allDigits \\ disallowed g p

disallowed :: Grid -> Pos -> Set Digit
disallowed g p =
  Set.fromList [ d
      | q <- Set.toList (peers p)
      , d <- (case lookG g q of Just d -> [d]; Nothing -> [])
      ]

peers :: Pos -> Set Pos
peers p = (hor p `union` vert p `union` box p) \\ singleton p

hor :: Pos -> Set Pos
hor Pos{y} = Set.fromList [Pos{x,y} | x <- [1..9] ]

vert :: Pos -> Set Pos
vert Pos{x} = Set.fromList [Pos{x,y} | y <- [1..9] ]

box :: Pos -> Set Pos
box Pos{x=x0,y=y0} = do
  let x' = (x0-1) `div` 3
  let y' = (y0-1) `div` 3
  Set.fromList [Pos{x=3*x'+xo,y=3*y'+yo} | xo <- [1,2,3], yo <- [1,2,3] ]


newtype Grid = Grid (Map Pos Digit)

filled :: Grid -> Pos -> Bool
filled g p = case lookG g p of Just{} -> True; Nothing -> False

lookG :: Grid -> Pos -> Maybe Digit
lookG (Grid m) p = Map.lookup p m

extendG :: Grid -> (Pos,Digit) -> Grid
extendG (Grid m) (p,d) = Grid (Map.insert p d m)

instance Show Grid where
  show (Grid m) = do
    let
      look :: Pos -> String
      look p = case Map.lookup p m of Just d -> show d; Nothing -> "."
    concat
      [ concat [ look pos
               | x <- [1..9]
               , let pos = Pos {x,y}
               ] ++ "\n"
      | y <- [1..9]
      ]

parseGivens :: String -> Grid
parseGivens s = do
  Grid $ Map.fromList $ [ (Pos {x,y},mkDigit c)
                        | (y,cs) <- zip [1..] $ lines s
                        , (x,c) <- zip [1..] cs
                        , c /= '.' ]


data Pos = Pos { x, y :: Int } deriving (Eq,Ord)

instance Show Pos where show Pos{x,y} = show (x,y)

allPos :: [Pos]
allPos = [ Pos {x,y} | y <- [1..9], x <- [1..9] ]


newtype Digit = Digit Int deriving (Eq,Ord)

mkDigit :: Char -> Digit
mkDigit c = do
  let ord0 = ord '0'
  let n = ord c - ord0
  if n<1 || n>9 then error (show ("mkDigit",c,n)) else Digit n

instance Show Digit where show (Digit d) = show d

allDigits :: Set Digit
allDigits = Set.fromList [ mkDigit n | n <- ['1' ..'9'] ]
