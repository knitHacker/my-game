{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs
    ( Configs(..)
    , GameConfigs(..)
    , initConfigs
    , ConfigsRead(..)
    , TextureCfg(..)
    , ItemCfg(..)
    , CharacterCfg(..)
    , CharacterMovement(..)
    , CharacterHitBoxes(..)
    , AreaCfg(..)
    , PositionCfg(..)
    , BarrierCfg(..)
    ) where

import Control.Monad ()
import System.IO ()
import Paths_my_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict )
import Data.Aeson.Types ( FromJSON, ToJSON )
import Data.Either ()
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GameState.Collision.BoundBox ( BoundBox )

import Env.Files    (getGameFullPath)

configFile :: FilePath
configFile = "data/configs/game.json"


texturesFile :: FilePath
texturesFile = "data/configs/textures.json"



data TextureCfg = TextureCfg
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON TextureCfg
instance ToJSON TextureCfg


data CharacterHitBoxes = CharHB
    { frontHitbox :: BoundBox
    , sideHitbox :: BoundBox
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON CharacterHitBoxes
instance ToJSON CharacterHitBoxes


data CharacterMovement = CharacterMovement
    { moveStep :: Int
    , stepRate :: Word32
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON CharacterMovement
instance ToJSON CharacterMovement


data CharacterCfg = CharacterCfg
    { charHitBox :: CharacterHitBoxes
    , charMovement :: CharacterMovement
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON CharacterCfg
instance ToJSON CharacterCfg


data ItemCfg = ItemCfg
    { itemHitBox :: BoundBox
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON ItemCfg
instance ToJSON ItemCfg

data PositionCfg = PosCfg
    { x :: Int
    , y :: Int
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON PositionCfg
instance ToJSON PositionCfg

data AreaCfg = AreaCfg
    { barriers :: M.Map T.Text PositionCfg
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON AreaCfg
instance ToJSON AreaCfg

data BarrierCfg = BarrierCfg
    { mainHitBox :: BoundBox
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON BarrierCfg
instance ToJSON BarrierCfg


data GameConfigs = GameConfigs
    { debug :: Bool
    , debugOutlineTexture :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    , characters :: M.Map T.Text CharacterCfg
    , areas :: M.Map T.Text AreaCfg
    , barrier_definitions :: M.Map T.Text BarrierCfg
    , items :: M.Map T.Text ItemCfg
    } deriving (Generic, Show, Eq)


instance FromJSON GameConfigs
instance ToJSON GameConfigs


data Configs = Configs
    { textureCfgs :: M.Map T.Text TextureCfg
    , gameCfgs :: GameConfigs
    } deriving (Generic, Show, Eq)

instance FromJSON Configs
instance ToJSON Configs


initConfigs :: IO Configs
initConfigs = do
    gPath <- getGameFullPath configFile
    configsM <- eitherDecodeFileStrict gPath
    case configsM of
        Left err -> error ("Failed to parse config file: " ++ (show err))
        Right configs -> do
            tPath <- getGameFullPath texturesFile
            texturesM <- eitherDecodeFileStrict tPath
            case texturesM of
                Left err -> error ("Failed to parse texture file: " ++ (show err))
                Right textures -> return $ Configs textures configs


class Monad m => ConfigsRead m where
    readConfigs :: m GameConfigs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug cfgs
