{-# LANGUAGE DeriveGeneric #-}

module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    ) where

import Control.Monad
import System.IO
import Paths_my_game
import GHC.Generics
import Data.Aeson
import Data.Either
import System.Directory

configFile :: FilePath
configFile = "data/configs/game.json"

data Configs = Configs
    { debug :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    } deriving (Generic, Show, Eq)


instance ToJSON Configs
instance FromJSON Configs

initConfigs :: IO Configs
initConfigs = do
    path <- getDataFileName configFile
    fileExists <- doesFileExist path
    if fileExists
    then do
        configsM <- eitherDecodeFileStrict path
        print configsM
        return $ fromRight defaultConfigs configsM
    else return defaultConfigs
    where
        defaultConfigs = Configs True 20 20 1000 800


class Monad m => ConfigsRead m where
    readConfigs :: m Configs




