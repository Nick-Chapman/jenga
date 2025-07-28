module UserMain (main) where

import Data.List.Ordered (nubSort)
import Engine (engineMain)
import Interface (G(..),Key(..),Loc(..),What(..))
import MakeStyle qualified (elaborate)
import StdBuildUtils ((</>))

main :: IO ()
main = engineMain $ \args -> do
  configs <- findConfigs args
  parallel_ [ MakeStyle.elaborate (Key config) | config <- configs ]

parallel_ :: [G ()] -> G ()
parallel_ = \case
  [] -> pure ()
  [g] -> g
  g:gs -> (\((),()) -> ()) <$> GPar g (parallel_ gs)

findConfigs :: [String] -> G [Loc]
findConfigs args = do
  let args' = case args of [] -> ["."]; _ -> args
  configs <- concat <$> sequence [ findFrom (Loc arg) | arg <- args' ]
  pure (nubSort configs)

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
  _ -> False
