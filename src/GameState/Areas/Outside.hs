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
    )
import InputState ( Direction(DDown) )
import GameState.Types
    ( Background(Background, backCollisions, backArea)
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
    )
import OutputHandles.Types
    ( OutputHandles(textures),
      TextureEntry(textureWidth, textureHeight) )
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

initItems :: GameConfigs -> OutputHandles -> Background -> RTree (CollisionType, Unique) -> IO (ItemManager, RTree (CollisionType, Unique))
initItems cfgs outs back cm = do
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
        bars = backCollisions back
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
initBackground :: GameConfigs -> OutputHandles -> IO Background
initBackground gCfgs outs = do
    let (barrs, cm) = foldl (\(b, c) (name, aCfg) -> insertBarrier name aCfg barrCfgs (textures outs) b c) (mempty, mempty) areaBarr
    return $ Background backT 0 0 barrs cm
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

initOutsideArea :: GameConfigs -> OutputHandles -> Player -> IO GameArea
initOutsideArea cfgs outs player = do
    back <- initBackground cfgs outs
    let pm = error "george smells" --  M.singleton george george
        cm = mempty
    (im, cm') <- initItems cfgs outs back cm
    let player' = updatePlayerPosition player 0 0 DDown
    return $ GameArea back player' (initNPC cfgs outs 20 10) im pm cm'

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)
