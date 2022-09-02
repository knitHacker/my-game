{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState
import GameState.Types
import OutputHandles.Types

import qualified Data.Text as T
import qualified SDL

import Utils

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class


initPlayer :: OutputHandles -> Player
initPlayer outs = Player ((textures outs) ! "character") (0, 10) Nothing mempty

initItems :: Configs -> IO ItemManager
initItems cfgs = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition cfgs
    return $ ItemManager $ M.fromList $ zip itemPos $ repeat $ Item Blob
    where
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs
        boardSize = boardWidth * boardHeight
        minItems = div boardSize 20
        maxItems = div boardSize 10


initBackground :: OutputHandles -> Background
initBackground outs = Background ((textures outs) ! "outside") 0 0


initGameState :: Configs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    items <- initItems cfgs
    return $ GameState (initBackground outs) (initPlayer outs) items World

randomPosition :: (MonadIO m) => Configs -> m (Int, Int)
randomPosition cfgs = do
    xPos <- randomValue 0 (boardWidth - 1)
    yPos <- randomValue 0 (boardHeight - 1)
    if (xPos, yPos) == (0, 0)
        then randomPosition cfgs
        else return (xPos, yPos)
    where
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs


updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    gs <- readGameState
    inputs <- readInputState
    player' <- case inputStateDirection inputs of
        Nothing -> return $ (gameStatePlayer gs) { playerMovement = Nothing }
        Just dir -> return $ updatePlayer (background gs) (gameStatePlayer gs) dir
    let background' = updateBackground cfgs (background gs) player'
    return $ collisionCheck (gs { background = background', gameStatePlayer = player' })


updateBackground :: Configs -> Background -> Player -> Background
updateBackground cfgs back player = back { xOffset = getOffset playerX windowX xMax
                                         , yOffset = getOffset playerY windowY yMax
                                         }
    where
        windowX = boardSizeX cfgs
        windowY = boardSizeY cfgs
        xMax = textureWidth $ area back
        yMax = textureHeight $ area back
        (playerX, playerY) = playerPosition $ player
        getOffset player window areaMax
            | player < (div window 2) = 0
            | player > areaMax - (div window 2) = areaMax - window
            | otherwise = player - (div window 2)

collisionCheck :: GameState -> GameState
collisionCheck gs =
    case M.lookup playerPos items of
        Nothing -> gs
        Just item ->
            let items' = M.delete playerPos items
                player' = player { playerItems = (M.insertWith update item 1 (playerItems player)) }
            in gs { gameStatePlayer = player', gameStateItemManager = (ItemManager items') }
    where
        items = gameItems $ gameStateItemManager gs
        player = gameStatePlayer gs
        playerPos = playerPosition $ player
        update old new = old + new


updatePlayer :: Background -> Player -> Direction -> Player
updatePlayer back player@(Player cfg pos (Just (d, l)) items) newDir
    | d == newDir && l >= 2 = player {playerPosition = newPosition back player newDir, playerMovement = Just (newDir, 0) }
    | d == newDir = player {playerMovement = Just (d, l + 1)}
    | otherwise = player { playerPosition = newPosition back player newDir, playerMovement = Just (newDir, 0) }
updatePlayer back player dir = player { playerPosition = newPosition back player dir, playerMovement = Just (dir, 0) }


newPosition :: Background -> Player -> Direction -> (Int, Int)
newPosition back player dir = (x'', y'')
    where
        charSizeX = textureWidth $ playerTexture player
        charSizeY = textureHeight $ playerTexture player
        (xMove, yMove) = updatePosition dir
        xMax = (textureWidth $ area back) - charSizeX
        yMax = (textureHeight $ area back) - charSizeY
        (x, y) = playerPosition player
        x' = x + xMove
        y' = y + yMove
        x'' = if x' < xMax  && x' >= 0 then x' else x
        y'' = if y' < yMax && y' >= 0 then y' else y


updatePosition :: Direction -> (Int, Int)
updatePosition DUp = (0, -2)
updatePosition DDown = (0, 2)
updatePosition DLeft = (-2, 0)
updatePosition DRight = (2, 0)
