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

import GameState.Item

mainCharName :: T.Text
mainCharName = "main_character"

npcName :: T.Text
npcName = "dog"

mushrooms :: [T.Text]
mushrooms = ["fly_agaric_mushroom", "mushroom"]

instance Show Unique where
    show = show . hashUnique

initNPC :: GameConfigs -> OutputHandles -> NPCManager
initNPC cfgs outs = NPCManager $ Player playCfgs playState
    where
        playCfgs = PlayerCfg textureEntry hb cc
        playState = PlayerState (startX, startY) (PlayerStanding DDown 0) mempty
        charCfgs = characters cfgs ! npcName
        textureEntry = textures outs ! npcName
        hb = charHitBox charCfgs
        cc = charMovement charCfgs
        startX = 20
        startY = 10


initPlayer :: GameConfigs -> OutputHandles -> Player
initPlayer cfgs outs = Player playCfgs playState
    where
        playCfgs = PlayerCfg textureEntry hb cc
        playState = PlayerState (startX, startY) (PlayerStanding DDown 0) mempty
        charCfgs = characters cfgs ! mainCharName
        textureEntry = textures outs ! mainCharName
        hb = charHitBox charCfgs
        cc = charMovement charCfgs
        startX = 0
        startY = 0

initItems :: GameConfigs -> OutputHandles -> Background -> RTree Unique -> IO (ItemManager, RTree Unique)
initItems cfgs outs back cm = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight mIW mIH
    itemNamesIds <- replicateM numberOfItems $randomValue 0 (length mushrooms - 1)
    uniqs <- replicateM numberOfItems newUnique
    let itemChoices = fmap (\index -> itemOptions !! index) itemNamesIds
    return $ insertItems (zipWith3 (,,) uniqs itemChoices itemPos) bars cm
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

insertItems :: [(Unique, Item, (Int, Int))] -> RTree Unique -> RTree Unique -> (ItemManager, RTree Unique)
insertItems info bars cm = foldr (\(u, i, pos) (im, rt) ->  insertItem i bars u pos (im, rt)) (ItemManager mempty Nothing, cm) info

insertItem :: Item -> RTree Unique -> Unique -> (Int, Int) -> (ItemManager, RTree Unique) -> (ItemManager, RTree Unique)
insertItem item bars un (x, y) (im, cm) =
    case getCollision hb' bars of
        [] -> case getCollision hb' cm of
            [] ->
                let im' = M.insert un (ItemState item (Just (x, y))) (itemMap im)
                    t = itemTexture item
                    cm' = insert hb' un cm
                in (im { itemMap=im' }, cm')
            _ -> (im, cm)
        _ -> (im, cm)
    where
        hb = itemHb item
        hb' = translate x y hb


-- bad literals in code
initBackground :: GameConfigs -> OutputHandles -> IO Background
initBackground gCfgs outs = do
    uns <- replicateM (length areaCfg) newUnique
    let (barrs, cm) = foldl (\(b, c) (un, (name, aCfg)) -> insertBarrier un name aCfg barrCfgs (textures outs) b c) (mempty, mempty) (zip uns areaCfg)
    return $ Background backT 0 0 barrs cm
    where
        name = "outside"
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


initOutsideArea :: GameConfigs -> OutputHandles -> IO GameArea
initOutsideArea cfgs outs = do
    back <- initBackground cfgs outs
    (im, cm) <- initItems cfgs outs back mempty
    return $ GameArea back (initPlayer cfgs outs) (initNPC cfgs outs) im cm

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)
