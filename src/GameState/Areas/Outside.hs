{-# LANGUAGE OverloadedStrings #-}
module GameState.Areas.Outside
    ( initOutsideArea
    ) where

import Control.Monad
import Configs
import InputState
import GameState.Types
import OutputHandles.Types
import GameState.Collision

import qualified Data.Text as T
import qualified SDL

import Utils

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class
import Data.Unique

import GameState.Collision.BoundBox
import GameState.Collision.RTree


mainCharName :: T.Text
mainCharName = "dog"

instance Show Unique where
    show = show . hashUnique


initPlayer :: GameConfigs -> OutputHandles -> Player
initPlayer cfgs outs = Player playCfgs playState
    where
        playCfgs = PlayerCfg textureEntry hb cc
        playState = PlayerState (startX, startY) (PlayerStanding DDown) mempty
        charCfgs = characters cfgs ! mainCharName
        textureEntry = textures outs ! mainCharName
        hb = charHitBox charCfgs
        cc = charMovement charCfgs
        startX = 0
        startY = 0

initItems :: GameConfigs -> OutputHandles -> Background -> RTree Unique -> IO (ItemManager, RTree Unique)
initItems cfgs outs back cm = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight iW iH
    uniqs <- replicateM numberOfItems newUnique
    return $ insertItems uniqs bars cm (Item mushroomEntry hb itemName) itemPos
    where
        itemName = "mushroom"
        mushroomEntry = textures outs ! itemName
        hb = itemHitBox $ items cfgs ! itemName
        bars = backCollisions back
        backT = backArea back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 25
        maxItems = 50
        iW = textureWidth mushroomEntry
        iH = textureHeight mushroomEntry

insertItems :: [Unique] -> RTree Unique -> RTree Unique -> Item -> [(Int, Int)] -> (ItemManager, RTree Unique)
insertItems uniqs bars cm item positions = foldr (uncurry (insertItem item bars)) (mempty, cm) $ zip uniqs positions

insertItem :: Item -> RTree Unique -> Unique -> (Int, Int) -> (ItemManager, RTree Unique) -> (ItemManager, RTree Unique)
insertItem item bars un (x, y) (im, cm) =
    case getCollision hb' bars of
        [] -> case getCollision hb' cm of
            [] ->
                let im' = M.insert un (ItemState item (Just (x, y))) im
                    t = itemTexture item
                    cm' = insert hb' un cm
                in (im', cm')
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
        areaCfg = M.toList $ barriers ((areas gCfgs) ! name)
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
    return $ GameArea back (initPlayer cfgs outs) im cm

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)
