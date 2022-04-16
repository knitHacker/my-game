module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    ) where

import Control.Monad


data Configs = Configs
    { configsDebug :: Bool
    , configsBoardSize :: (Int, Int)
    , configsScreenSize :: (Int, Int)
    } deriving (Show, Eq)


initConfigs :: IO Configs
initConfigs = return $ Configs True (10, 10) (800, 600)


class Monad m => ConfigsRead m where
    readConfigs :: m Configs

