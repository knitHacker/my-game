module GameState
    ( initGameState
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState
import GameState.Types

import Utils

import qualified Data.Map.Strict as M
import Control.Monad.IO.Class

initPlayer :: Player
initPlayer = Player (0, 0) Nothing mempty

initItems :: Configs -> IO ItemManager
initItems cfgs = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition cfgs
    return $ ItemManager $ M.fromList $ zip itemPos $ repeat $ Item Blob
    where
        (boardWidth, boardHeight) = configsBoardSize cfgs
        boardSize = boardWidth * boardHeight
        minItems = div boardSize 20
        maxItems = div boardSize 10


initGameState :: Configs -> IO GameState
initGameState cfgs = do
    items <- initItems cfgs
    return $ GameState initPlayer items



randomPosition :: (MonadIO m) => Configs -> m (Int, Int)
randomPosition cfgs = do
    xPos <- randomValue 0 (boardWidth - 1)
    yPos <- randomValue 0 (boardHeight - 1)
    if (xPos, yPos) == (0, 0)
        then randomPosition cfgs
        else return (xPos, yPos)
    where
        (boardWidth, boardHeight) = configsBoardSize cfgs


updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    gs <- readGameState
    inputs <- readInputState
    player' <- case inputStateDirection inputs of
        Nothing -> return $ (gameStatePlayer gs) { playerMovement = Nothing }
        Just dir -> return $ updatePlayer cfgs (gameStatePlayer gs) dir
    return $ collisionCheck (gs { gameStatePlayer = player' })


collisionCheck :: GameState -> GameState
collisionCheck gs =
    case M.lookup playerPos items of
        Nothing -> gs
        Just item ->
            let items' = M.delete playerPos items
                player' = player { playerItems = (M.insertWith update item 1 (playerItems player)) }
            in GameState player' (ItemManager items')
    where
        items = gameItems $ gameStateItemManager gs
        player = gameStatePlayer gs
        playerPos = playerPosition $ player
        update old new = old + new


updatePlayer :: Configs -> Player -> Direction -> Player
updatePlayer cfgs player dir
    | playerMovement player == Just dir = player
    | otherwise = player { playerPosition = (x'', y''), playerMovement = Just dir }
    where
        (xMove, yMove) = updatePosition dir
        (xMax, yMax) = configsBoardSize cfgs
        (x, y) = playerPosition player
        x' = x + xMove
        y' = y + yMove
        x'' = if x' < xMax  && x' >= 0 then x' else x
        y'' = if y' < yMax && y' >= 0 then y' else y

updatePosition :: Direction -> (Int, Int)
updatePosition DUp = (0, -1)
updatePosition DDown = (0, 1)
updatePosition DLeft = (-1, 0)
updatePosition DRight = (1, 0)
