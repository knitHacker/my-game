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
    ( Background(Background, backCollisions, backArea),
      ItemManager(..),
      ItemState(ItemState),
      Item(Item, itemHb, itemTexture),
      Player(Player),
      PlayerState(PlayerState),
      PlayerConfig(PlayerCfg),
      NPCManager(NPCManager),
      GameArea(GameArea),
      PlayerAction(PlayerStanding) )
import OutputHandles.Types
    ( OutputHandles(textures),
      TextureEntry(textureWidth, textureHeight) )
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

initBackground :: GameConfigs -> OutputHandles -> IO Background
initBackground gCfgs outs = do
    uns <- replicateM (length areaCfg) newUnique
    let (barrs, cm) = foldl (\(b, c) (un, (name, aCfg)) -> insertBarrier un name aCfg barrCfgs (textures outs) b c) (mempty, mempty) (zip uns areaCfg)
    return $ Background backT 0 0 barrs M.empty cm
    where
        name = "inside_house"
        areaCfg = M.toList $ barriers (areas gCfgs ! name)
        backT = textures outs ! name
        barrCfgs = barrier_definitions gCfgs


insertBarrier :: Unique -> T.Text -> PositionCfg -> M.Map T.Text BarrierCfg
              -> M.Map T.Text TextureEntry
              -> M.Map Unique ((Int, Int), TextureEntry) -> RTree Unique
              -> (M.Map Unique ((Int, Int), TextureEntry), RTree Unique)
insertBarrier un name aCfg barrCfgs texts barrs rt = (barrs', rt')
    where
        xPos = x aCfg
        yPos = y aCfg
        text = texts ! name
        barrs' = M.insert un ((xPos, yPos), text) barrs
        bCfg = barrCfgs ! name
        rt' = insert (translate xPos yPos (mainHitBox bCfg)) un rt


initInsideArea :: GameConfigs -> OutputHandles -> Player -> IO GameArea
initInsideArea cfgs outs player = do
    back <- initBackground cfgs outs
    let im = ItemManager mempty Nothing
        cm = mempty
        player' = updatePlayerPosition player 0 0 DDown
    return $ GameArea back player' (initNPC cfgs outs 20 10) im cm