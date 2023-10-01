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


-- bad literals in code
initPlayer :: Configs -> OutputHandles -> Player
initPlayer cfgs outs = Player textureEntry boundb (startX, startY) (Left DDown) mempty cc
    where
        charCfgs = characters cfgs ! mainCharName
        textureEntry = textures outs ! mainCharName
        hb = charHitBox charCfgs
        boundb = bb (hitboxX1 hb) (hitboxY1 hb) (hitboxX2 hb) (hitboxY2 hb)
        cc = charMovement charCfgs
        startX = 0
        startY = 0

initItems :: OutputHandles -> Background -> RTree Unique -> IO (ItemManager, RTree Unique)
initItems outs back cm = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight iW iH
    uniqs <- replicateM numberOfItems newUnique
    return $ insertItems uniqs cm (Item mushroomEntry Mushroom) itemPos
    where
        mushroomEntry = textures outs ! "mushroom"
        backT = backArea back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 25
        maxItems = 50
        iW = textureWidth mushroomEntry
        iH = textureHeight mushroomEntry

insertItems :: [Unique] -> RTree Unique -> Item -> [(Int, Int)] -> (ItemManager, RTree Unique)
insertItems uniqs cm item positions = foldr (uncurry (insertItem item)) (mempty, cm) $ zip uniqs positions

insertItem :: Item -> Unique -> (Int, Int) -> (ItemManager, RTree Unique) -> (ItemManager, RTree Unique)
insertItem item un (x, y) (im, cm) =
    case getCollision (bb x  y  1  1) cm of
        [] ->
            let im' = M.insert un (ItemState item (Just (x, y))) im
                t = itemTexture item
                cm' = insert (bb x y (textureWidth t) (textureHeight t)) un cm
            in (im', cm')
        _ -> (im, cm)


-- bad literals in code
initBackground :: OutputHandles -> IO Background
initBackground outs = do
    un <- newUnique
    let barrs = M.insert un ((pondX, pondY),  pond) mempty
        cm = insert (bb pondX pondY (textureWidth pond) (textureHeight pond)) un mempty
    return $ Background ((textures outs) ! "outside") 0 0 barrs cm
    where
        pondX = 150
        pondY = 100
        backT = textures outs ! "outside"
        pond = textures outs ! "pond"


initOutsideArea :: Configs -> OutputHandles -> IO GameArea
initOutsideArea cfgs outs = do
    back <- initBackground outs
    (im, cm) <- initItems outs back mempty
    return $ GameArea back (initPlayer cfgs outs) im cm

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)


