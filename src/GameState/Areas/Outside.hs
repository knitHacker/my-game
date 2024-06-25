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
    , CollisionEntry
    , Portal(..)
    , Barriers
    , ItemMap
    , Items(..)
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

initItems :: GameConfigs -> OutputHandles -> Background -> Barriers -> IO (Barriers, ItemManager)
initItems cfgs outs back bars = do
    (bars', im') <- initPortals cfgs outs bars im
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight mIW mIH
    itemNamesIds <- replicateM numberOfItems $randomValue 0 (length mushrooms - 1)
    uniqs <- replicateM numberOfItems newUnique
    let itemChoices = fmap (itemOptions !!) itemNamesIds
    return (bars', insertItems im' (zip3 uniqs itemChoices itemPos) bars')
    where
        im = ItemManager M.empty Nothing mempty
        itemOptions = fmap newItem mushrooms
        newItem :: T.Text -> Item
        newItem iT =
            let highlightName = iT `T.append` "_highlight"
                mushroomEntry = textures outs ! iT
                hightlightEntry = textures outs ! highlightName
                itemCfg = items cfgs ! iT
                hb = itemHitBox itemCfg
                iN = itemText itemCfg
            in Item iN mushroomEntry hightlightEntry hb iT
        backT = backArea back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 25
        maxItems = 50
        mIW = maximum $ fmap (textureWidth . itemTexture) itemOptions
        mIH = maximum $ fmap (textureHeight . itemTexture) itemOptions

insertItems :: ItemManager -> [(Unique, Item, (Int, Int))] -> Barriers -> ItemManager
insertItems im info bars = foldr (\(u, i, pos) im ->  insertItem i bars u pos im) im info

insertItem :: Item -> Barriers -> Unique -> (Int, Int) -> ItemManager -> ItemManager
insertItem item bars un (x, y) im =
    case getCollision hb' bars of
        [] -> case getCollision hb' cm of
            [] ->
                let im' = M.insert un (CollectItem $ ItemState item (Just (x, y))) (itemMap im)
                    t = itemTexture item
                    cm' = insert hb' un cm
                in im { itemMap=im', collisionMap = cm' }
            _ -> im { collisionMap = cm}
        _ -> im { collisionMap = cm }
    where
        cm = collisionMap im
        hb = itemHb item
        hb' = translate x y hb

-- bad literals in code
initBackground :: GameConfigs -> OutputHandles -> IO (Barriers, Background)
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
getAreaType "inside" = InsideArea
getAreaType "outside" = OutsideArea
getAreaType _ = error "Area type not found"

initPortals :: GameConfigs -> OutputHandles -> Barriers -> ItemManager -> IO (Barriers, ItemManager)
initPortals cfgs outs rt im = do
    un <- newUnique
    let ps = addPortal (textures outs) (rt, im) (un, InsideArea, (portalName, portalCfgs ! portalName))
    return ps
    where
        portalName = "house"
        areaCfg = areas cfgs ! "outside"
        portalCfgs = portals areaCfg

addPortal :: TextureMap -> (Barriers, ItemManager) -> (Unique, AreaLocation, (T.Text, PortalCfg)) -> (Barriers, ItemManager)
addPortal tm (barrs, im) (un, area, (n, pCfg)) = (barrs', im')
    where
        pm = itemMap im
        cm = collisionMap im
        loc = portalPosition pCfg
        barrs' = insertBarrier n (x loc, y loc) (bb 0 0 barW barH) barrs
        open = tm ! (n `T.append` "_open")
        close = tm ! (n `T.append` "_closed")
        barH = maximum $ fmap textureHeight [open, close]
        barW = maximum $ fmap textureWidth [open, close]
        pm' = M.insert un (PortalItem $ Portal area (x loc, y loc) hb close open) pm
        hb = portalHitBox pCfg
        cm' = insert (translate (x loc) (y loc) hb) un cm
        im' = im { itemMap = pm', collisionMap = cm' }

initOutsideArea :: GameConfigs -> OutputHandles -> Player -> IO GameArea
initOutsideArea cfgs outs player = do
    (bcm, back) <- initBackground cfgs outs
    (bcm', im) <- initItems cfgs outs back bcm
    let player' = updatePlayerPosition player 0 0 DDown
    return $ GameArea back player' (initNPC cfgs outs 20 10) im bcm'

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)
