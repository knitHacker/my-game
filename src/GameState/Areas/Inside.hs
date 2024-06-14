{-# LANGUAGE OverloadedStrings #-}
module GameState.Areas.Inside
    ( initInsideArea
    ) where

import Control.Monad ( replicateM )
import Configs
    ( AreaCfg(..)
    , BarrierCfg(..)
    , CharacterCfg(..)
    , GameConfigs(..)
    , ItemCfg(..)
    , PositionCfg(..)
    )
import InputState ( Direction(DDown) )
import GameState.Types
    ( Background(..)
    , ItemManager(..)
    , ItemState(ItemState)
    , Item(Item, itemHb, itemTexture)
    , Player(Player)
    , PlayerState(PlayerState)
    , PlayerConfig(PlayerCfg)
    , NPCManager(NPCManager)
    , GameArea(GameArea)
    , PlayerAction(PlayerStanding)
    , Barriers
    )
import OutputHandles.Types
    ( OutputHandles(textures)
    , TextureEntry(textureWidth, textureHeight)
    )
import GameState.Collision ()

import qualified Data.Text as T
import qualified SDL

import Utils ( randomValue )

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO )
import Data.Unique ( Unique, hashUnique, newUnique )

import GameState.Collision.BoundBox ( translate )
import GameState.Collision.RTree ( getCollision, insert, RTree )
import GameState.Player
    ( mainCharName
    , npcName
    , initNPC
    , initPlayer
    , updatePlayerPosition
    )
import GameState.Barrier

initBackground :: GameConfigs -> OutputHandles -> IO (Background, Barriers)
initBackground gCfgs outs = do
    let (barrs, cm) = foldl (\(b, c) (name, aCfg) -> insertBarriers name aCfg barrCfgs (textures outs) b c) (mempty, mempty) areaCfg
    return (Background backT 0 0 barrs, cm)
    where
        name = "inside_house"
        areaCfg = M.toList $ barriers (areas gCfgs ! name)
        backT = textures outs ! name
        barrCfgs = barrier_definitions gCfgs

initInsideArea :: GameConfigs -> OutputHandles -> Player -> IO GameArea
initInsideArea cfgs outs player = do
    (back, bcm) <- initBackground cfgs outs
    let im = ItemManager mempty Nothing
        cm = mempty
        player' = updatePlayerPosition player 0 0 DDown
    return $ GameArea back player' (initNPC cfgs outs 20 10) im mempty cm bcm
