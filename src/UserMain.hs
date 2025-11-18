module UserMain (main) where

import Data.List.Ordered (nubSort)
import Engine (engineMain)
import Interface (G(..),Key(..),Loc(..),What(..))
import MakeStyle qualified (elaborate)
import StdBuildUtils ((</>))

main :: IO ()
main = engineMain $ \withPromotion args -> do
  configs <- findConfigs args
  --GLog (show configs)
  sequence_ [ MakeStyle.elaborate withPromotion (Key config) | config <- configs ]

_parallel_ :: [G ()] -> G ()
_parallel_ = \case
  [] -> pure ()
  [g] -> g
  g:gs -> (\((),()) -> ()) <$> GPar g (_parallel_ gs)

findConfigs :: [String] -> G [Loc]
findConfigs args = do
  let args' = case args of [] -> ["."]; _ -> args
  configs <- concat <$> sequence [ findFrom (Loc arg) | arg <- args' ]
  pure (reverse $ nubSort configs) -- reverse so subdirs come earlier

findFrom :: Loc -> G [Loc]
findFrom loc = do
  GWhat loc >>= \case
    Missing -> pure []
    Link -> pure []
    File -> pure []
    Directory{entries} -> do
      case "build.jenga" `elem` entries of
        False -> configs
        True -> do
          let config = loc </> "build.jenga"
          (config:) <$> configs
      where
        configs = concat <$> sequence [ findFrom (loc </> e)
                                      | e <- entries , not (blockName e)]

blockName :: String -> Bool
blockName = \case
  ".git" -> True
  ".stack-work" -> True
  ".cache" -> True
  ",jenga" -> True
  "_build" -> True -- dune
  _ -> False
