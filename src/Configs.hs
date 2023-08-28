{-# LANGUAGE DeriveGeneric #-}

module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    , TextureCfg(..)
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


data TextureCfg = TextureCfg
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON TextureCfg
instance FromJSON TextureCfg

data Configs = Configs
    { debug :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    , characters :: M.Map T.Text TextureCfg
    , areas :: M.Map T.Text TextureCfg
    , items :: M.Map T.Text TextureCfg
    , barriers :: M.Map T.Text TextureCfg
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
        case configsM of
            Left err -> error ("Failed to parse config file" ++ (show err))
            Right configs -> return configs
    else error "Missing config file required for game"


class Monad m => ConfigsRead m where
    readConfigs :: m Configs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug cfgs




