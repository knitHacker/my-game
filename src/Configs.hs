{-# LANGUAGE DeriveGeneric #-}

module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    , TextureCfg(..)
    , HitBox(..)
    , CharacterCfg(..)
    , CharacterMovement(..)
    ) where

import Control.Monad
import System.IO
import Paths_my_game
import GHC.Generics
import Data.Aeson
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Env.Files    (getGameFullPath)

configFile :: FilePath
configFile = "data/configs/game.json"


data TextureCfg = TextureCfg
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON TextureCfg
instance FromJSON TextureCfg

data HitBox = HitBox
    { sideWidth :: Int
    , frontWidth :: Int
    , collideHeight :: Int
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON HitBox
instance FromJSON HitBox

data CharacterMovement = CharacterMovement
    { moveStep :: Int
    , stepRate :: Int
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON CharacterMovement
instance FromJSON CharacterMovement

data CharacterCfg = CharacterCfg
    { charTexture :: TextureCfg
    , charHitBox :: HitBox
    , charMovement :: CharacterMovement
    } deriving (Generic, Show, Eq, Ord)

instance ToJSON CharacterCfg
instance FromJSON CharacterCfg


data Configs = Configs
    { debug :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    , characters :: M.Map T.Text CharacterCfg
    , areas :: M.Map T.Text TextureCfg
    , items :: M.Map T.Text TextureCfg
    , barriers :: M.Map T.Text TextureCfg
    } deriving (Generic, Show, Eq)


instance ToJSON Configs
instance FromJSON Configs

initConfigs :: IO Configs
initConfigs = do
    path <- getGameFullPath configFile
    configsM <- eitherDecodeFileStrict path
    -- print configsM
    case configsM of
        Left err -> error ("Failed to parse config file" ++ (show err))
        Right configs -> return configs


class Monad m => ConfigsRead m where
    readConfigs :: m Configs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug cfgs
