{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs
    ( Configs(..)
    , initConfigs
    , ConfigsRead(..)
    , TextureCfg(..)
    , CharacterCfg(..)
    , CharacterMovement(..)
    , CharacterHitBoxes(..)
    ) where

import Control.Monad
import System.IO
import Paths_my_game
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Either
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GameState.Collision.BoundBox

import Env.Files    (getGameFullPath)

configFile :: FilePath
configFile = "data/configs/game.json"


data TextureCfg = TextureCfg
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON TextureCfg


data CharacterHitBoxes = CharHB
    { frontHitbox :: BoundBox
    , sideHitbox :: BoundBox
    } deriving (Show, Eq, Ord)

instance FromJSON CharacterHitBoxes where
    parseJSON (Object o) = do
        frontB <- o .: "frontHitbox"
        sideB <- o .: "sideHitbox"
        let parseBB hb = do
                x1 <- hb .: "x1"
                y1 <- hb .: "y1"
                x2 <- hb .: "x2"
                y2 <- hb .: "y2"
                return $ bb x1 y1 x2 y2
        fBB <- parseBB frontB
        sBB <- parseBB sideB
        return $ CharHB fBB sBB

    parseJSON invalid = prependFailure "parsing CharHB failed, "
            (typeMismatch "Object" invalid)

data CharacterMovement = CharacterMovement
    { moveStep :: Int
    , stepRate :: Word32
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON CharacterMovement

data CharacterCfg = CharacterCfg
    { charTexture :: TextureCfg
    , charHitBox :: CharacterHitBoxes
    , charMovement :: CharacterMovement
    } deriving (Generic, Show, Eq, Ord)

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


instance FromJSON Configs

initConfigs :: IO Configs
initConfigs = do
    path <- getGameFullPath configFile
    configsM <- eitherDecodeFileStrict path
    -- print configsM
    case configsM of
        Left err -> error ("Failed to parse config file: " ++ (show err))
        Right configs -> return configs


class Monad m => ConfigsRead m where
    readConfigs :: m Configs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug cfgs
