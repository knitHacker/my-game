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


instance Show Unique where
    show = show . hashUnique


-- bad literals in code
initPlayer :: OutputHandles -> Player
initPlayer outs = Player textureEntry (startX, startY) (Left DDown) mempty
    where
        textureEntry = textures outs ! "character"
        startX = 0
        startY = 0

initItems :: OutputHandles -> Background -> ObjectMap -> CollisionMap Unique -> IO (ItemManager, ObjectMap, CollisionMap Unique)
initItems outs back m cm = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight iW iH
    uniqs <- replicateM numberOfItems newUnique
    return $ insertItems uniqs m cm (Item mushroomEntry Mushroom) itemPos
    where
        mushroomEntry = textures outs ! "mushroom"
        backT = area back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 25
        maxItems = 50
        iW = textureWidth mushroomEntry
        iH = textureHeight mushroomEntry

insertItems :: [Unique] -> ObjectMap -> CollisionMap Unique -> Item -> [(Int, Int)] -> (ItemManager, ObjectMap, CollisionMap Unique)
insertItems uniqs m cm item positions = foldr (uncurry (insertItem item)) (mempty, m, cm) $ zip uniqs positions

insertItem :: Item -> Unique -> (Int, Int) -> (ItemManager, ObjectMap, CollisionMap Unique) -> (ItemManager, ObjectMap, CollisionMap Unique)
insertItem item un (x, y) (im, m, cm) =
    case detectCollision (x, y, 1, 1) cm of
        [] ->
            let m' = M.insert un BoardItem m
                im' = M.insert un (ItemState item (Just (x, y))) im
                t = itemTexture item
                cm' = insertCollision (x,y,textureWidth t,textureHeight t,un) cm
            in (im', m', cm')
        _ -> (im, m, cm)


-- bad literals in code
initBackground :: OutputHandles -> IO (Background, ObjectMap, CollisionMap Unique)
initBackground outs = do
    un <- newUnique
    let barrs = M.insert un ((pondX, pondY),  pond) mempty
        om = M.insert un BoardBarrier mempty
        cm = insertCollision (pondX, pondY, textureWidth pond, textureHeight pond, un) mempty
    return (Background ((textures outs) ! "outside") 0 0 barrs, om, cm)
    where
        pondX = 150
        pondY = 100
        backT = textures outs ! "outside"
        pond = textures outs ! "pond"


initOutsideArea :: Configs -> OutputHandles -> IO GameArea
initOutsideArea cfgs outs = do
    (back, m, cm) <- initBackground outs
    (im, m', cm') <- initItems outs back m cm
    return $ GameArea back (initPlayer outs) im m' cm'

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)


