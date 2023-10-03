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


initPlayer :: Configs -> OutputHandles -> Player
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

initItems :: Configs -> OutputHandles -> Background -> RTree Unique -> IO (ItemManager, RTree Unique)
initItems cfgs outs back cm = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight iW iH
    uniqs <- replicateM numberOfItems newUnique
    return $ insertItems uniqs bars cm (Item mushroomEntry hb itemName) itemPos
    where
        itemName = "fern"
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
initBackground :: OutputHandles -> IO Background
initBackground outs = do
    un <- newUnique
    let barrs = M.insert un ((pondX, pondY),  pond) mempty
        cm = insert (bb pondX pondY (pondX + (textureWidth pond)) (pondY + (textureHeight pond))) un mempty
    return $ Background ((textures outs) ! "outside") 0 0 barrs cm
    where
        pondX = 150
        pondY = 100
        backT = textures outs ! "outside"
        pond = textures outs ! "pond"


initOutsideArea :: Configs -> OutputHandles -> IO GameArea
initOutsideArea cfgs outs = do
    back <- initBackground outs
    (im, cm) <- initItems cfgs outs back mempty
    return $ GameArea back (initPlayer cfgs outs) im cm

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)
