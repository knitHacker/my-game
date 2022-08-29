{-# LANGUAGE DeriveGeneric #-}

module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    , AreaCfg(..)
    , CharacterCfg(..)
    ) where

import Control.Monad
import System.IO
import Paths_my_game
import GHC.Generics
import Data.Aeson
import Data.Either
import System.Directory
import qualified Data.Map.Strict as M
import qualified Data.Text as T

configFile :: FilePath
configFile = "data/configs/game.json"

data CharacterCfg = CharacterCfg
    { characterFile :: Maybe FilePath
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON CharacterCfg
instance FromJSON CharacterCfg

data AreaCfg = Area
    { sizeX :: Int
    , sizeY :: Int
    , areaFile :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON AreaCfg
instance FromJSON AreaCfg

data Configs = Configs
    { debug :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    , character :: CharacterCfg
    , areas :: M.Map T.Text AreaCfg
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
        defaultConfigs = Configs True 20 20 1000 800 defaultChar mempty
        defaultChar = CharacterCfg Nothing


class Monad m => ConfigsRead m where
    readConfigs :: m Configs




