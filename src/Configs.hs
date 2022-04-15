module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    ) where

import Control.Monad

data Configs = Configs
    { configsDebug :: Bool
    , configsBoardSize :: (Int, Int)
    } deriving (Show, Eq)


initConfigs :: IO Configs
initConfigs = return $ Configs True (5, 5)


class Monad m => ConfigsRead m where
    readConfigs :: m Configs

