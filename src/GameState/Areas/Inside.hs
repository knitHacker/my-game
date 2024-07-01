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
import InputState ( Direction(DUp) )
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

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO )

import GameState.Player
    ( mainCharName
    , npcName
    , initNPC
    , initPlayer
    , updatePlayerPosition
    )
import GameState.Barrier
import GameState.Background
import GameState.Types

initInsideArea :: GameConfigs -> OutputHandles -> Player -> IO GameArea
initInsideArea cfgs outs player = do
    return $ GameArea back' player' (initNPC cfgs outs (pX + 20) (pY + 10)) im bcm
    where
        name = "inside_house"
        areaCfg = M.toList $ barriers (areas cfgs ! name)
        backT = textures outs ! name
        width = fromIntegral $ textureWidth $ backArea back
        height = fromIntegral $ textureHeight $ backArea back
        barrCfgs = barrier_definitions cfgs
        (barrs, bcm) = foldl (\(b, c) (name, aCfg) -> insertBarriers name aCfg barrCfgs (textures outs) b c) (mempty, mempty) areaCfg
        back = Background backT 0 0 barrs
        playerHeight = textureHeight $ playerTexture $ playerCfgs player
        pX = width `div` 2
        pY = height - playerHeight
        player' = updatePlayerPosition player pX pY DUp
        back' = updateBackground cfgs back player'
        im = ItemManager mempty Nothing mempty -- so far no items in this area
