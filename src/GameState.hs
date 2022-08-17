{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState
import GameState.Types

import qualified Data.Text as T
import SDL

import Utils

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class

initPlayer :: Player
initPlayer = Player (0, 0) Nothing mempty

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


initGameState :: Configs -> IO GameState
initGameState cfgs = do
    items <- initItems cfgs
    return $ GameState background initPlayer items World
    where
        area = ((areas cfgs) ! "outside")
        background = Background area 0 0

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
        xMax = sizeX $ area back
        yMax = sizeY $ area back
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
updatePlayer back player dir
    | playerMovement player == Just dir = player
    | otherwise = player { playerPosition = (x'', y''), playerMovement = Just dir }
    where
        (xMove, yMove) = updatePosition dir
        xMax = sizeX $ area back
        yMax = sizeY $ area back
        (x, y) = playerPosition player
        x' = x + xMove
        y' = y + yMove
        x'' = if x' < xMax  && x' >= 0 then x' else x
        y'' = if y' < yMax && y' >= 0 then y' else y

updatePosition :: Direction -> (Int, Int)
updatePosition DUp = (0, -5)
updatePosition DDown = (0, 5)
updatePosition DLeft = (-5, 0)
updatePosition DRight = (5, 0)
