{-# LANGUAGE OverloadedStrings #-}
module GameState.Areas.Outside
    ( initOutsideArea
    ) where

import Control.Monad ( replicateM )
import Configs
    ( AreaCfg(..)
    , BarrierCfg(..)
    , CharacterCfg(..)
    , GameConfigs(..)
    , ItemCfg(..)
    , PositionCfg(..)
    , PortalCfg (..)
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
    , AreaLocation(..)
    , CollisionType(..)
    , CollisionEntry
    , Portal(..)
    )
import OutputHandles.Types
    ( OutputHandles(textures)
    , TextureEntry(textureWidth, textureHeight)
    , TextureMap
    )
import GameState.Collision ()
import GameState.Player
    ( mainCharName
    , npcName
    , initNPC
    , initPlayer
    , updatePlayerPosition
    )

import qualified Data.Text as T
import qualified SDL

import Utils ( randomValue )

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO )
import Data.Unique ( Unique, hashUnique, newUnique )

import GameState.Collision.BoundBox ( bb, translate )
import GameState.Collision.RTree ( getCollision, insert, RTree )

import GameState.Item
import GameState.Barrier

mushrooms :: [T.Text]
mushrooms = ["fly_agaric_mushroom", "mushroom"]

initItems :: GameConfigs -> OutputHandles -> Background -> RTree () -> RTree (CollisionType, Unique) -> IO (ItemManager, RTree (CollisionType, Unique))
initItems cfgs outs back bars cm = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight mIW mIH
    itemNamesIds <- replicateM numberOfItems $randomValue 0 (length mushrooms - 1)
    uniqs <- replicateM numberOfItems newUnique
    let itemChoices = fmap (itemOptions !!) itemNamesIds
    return $ insertItems (zip3 uniqs itemChoices itemPos) bars cm
    where
        itemOptions = fmap newItem mushrooms
        newItem :: T.Text -> Item
        newItem iT =
            let highlightName = iT `T.append` "_highlight"
                mushroomEntry = textures outs ! iT
                hightlightEntry = textures outs ! highlightName
                itemCfg = items cfgs ! iT
                hb = itemHitBox itemCfg
                iN = itemText itemCfg
            in Item iN mushroomEntry hightlightEntry hb iT pickupOnCollision
        backT = backArea back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 25
        maxItems = 50
        mIW = maximum $ fmap (textureWidth . itemTexture) itemOptions
        mIH = maximum $ fmap (textureHeight . itemTexture) itemOptions
        portals = undefined

insertItems :: [(Unique, Item, (Int, Int))] -> RTree () -> RTree (CollisionType, Unique) -> (ItemManager, RTree (CollisionType, Unique))
-- why is this a foldr? if i remember come back and add a comment
insertItems info bars cm = foldr (\(u, i, pos) (im, rt) ->  insertItem i bars u pos (im, rt)) (ItemManager mempty Nothing, cm) info

insertItem :: Item -> RTree () -> Unique -> (Int, Int) -> (ItemManager, RTree (CollisionType, Unique)) -> (ItemManager, RTree (CollisionType, Unique))
insertItem item bars un (x, y) (im, cm) =
    case getCollision hb' bars of
        [] -> case getCollision hb' cm of
            [] ->
                let im' = M.insert un (ItemState item (Just (x, y))) (itemMap im)
                    t = itemTexture item
                    cm' = insert hb' (ItemCollision, un) cm
                in (im { itemMap=im' }, cm')
            _ -> (im, cm)
        _ -> (im, cm)
    where
        hb = itemHb item
        hb' = translate x y hb


-- bad literals in code
initBackground :: GameConfigs -> OutputHandles -> IO (RTree (), Background)
initBackground gCfgs outs = do
    let (barrs, cm) = foldl (\(b, c) (name, aCfg) -> insertBarriers name aCfg barrCfgs (textures outs) b c) (mempty, mempty) areaBarr
    return (cm, Background backT 0 0 barrs)
    where
        name = "outside"
        areaCfg = areas gCfgs ! name
        areaBarr = M.toList $ barriers areaCfg
        backT = textures outs ! name
        barrCfgs = barrier_definitions gCfgs
        portalCfgs = M.toList $ portals areaCfg
        portalEntries = foldl (\m (n, bb) -> M.insert (getAreaType n) bb m) M.empty portalCfgs

getAreaType :: T.Text -> AreaLocation
getAreaType "inside" = Inside
getAreaType "outside" = Outside
getAreaType _ = error "Area type not found"

initPortals :: GameConfigs -> OutputHandles -> RTree () -> IO (RTree (), M.Map Unique Portal, RTree CollisionEntry)
initPortals cfgs outs rt = do
    un <- newUnique
    return $ addPortal (textures outs) (rt, M.empty, mempty) (un, Inside, (portalName, portalCfgs ! portalName))
    where
        portalName = "house"
        areaCfg = areas cfgs ! "outside"
        portalCfgs = portals areaCfg

addPortal :: TextureMap -> (RTree (), M.Map Unique Portal, RTree CollisionEntry) -> (Unique, AreaLocation, (T.Text, PortalCfg)) -> (RTree (), M.Map Unique Portal, RTree CollisionEntry)
addPortal tm (barrs, pm, cm) (un, area, (n, pCfg)) = (barrs', pm', cm')
    where
        loc = portalPosition pCfg
        barrs' = insertBarrier n (x loc, y loc) (bb 0 0 barW barH) barrs
        open = tm ! (n `T.append` "_open")
        close = tm ! (n `T.append` "_closed")
        barH = maximum $ fmap textureHeight [open, close]
        barW = maximum $ fmap textureWidth [open, close]
        pm' = M.insert un (Portal area (x loc, y loc) hb False close open) pm
        hb = translate (x loc) (y loc) $ portalHitBox pCfg
        cm' = insert hb (PortalCollision, un) cm

initOutsideArea :: GameConfigs -> OutputHandles -> Player -> IO GameArea
initOutsideArea cfgs outs player = do
    (bcm, back) <- initBackground cfgs outs
    (bcm', pm, cm) <- initPortals cfgs outs bcm
    (im, cm') <- initItems cfgs outs back bcm' cm
    let player' = updatePlayerPosition player 0 0 DDown
    return $ GameArea back player' (initNPC cfgs outs 20 10) im pm cm' bcm'

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)
