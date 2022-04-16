module GameState
    ( GameState(..)
    , initGameState
    , GameStateRead(..)
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState

import Utils


import Control.Monad.IO.Class

data GameState = GameState
    { gameStateUserState :: (Int, Int)
    , gameStateUserMovement :: Maybe Direction
    , gameStateItemPositions :: [(Int, Int)]
    } deriving (Show, Eq)


initGameState :: Configs -> IO GameState
initGameState cfgs = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition cfgs
    return $ GameState (0,0) Nothing itemPos
    where
        (boardWidth, boardHeight) = configsBoardSize cfgs
        boardSize = boardWidth * boardHeight
        minItems = div boardSize 20
        maxItems = div boardSize 10



randomPosition :: (MonadIO m) => Configs -> m (Int, Int)
randomPosition cfgs = do
    xPos <- randomValue 0 (boardWidth - 1)
    yPos <- randomValue 0 (boardHeight - 1)
    return (xPos, yPos)
    where
        (boardWidth, boardHeight) = configsBoardSize cfgs

class Monad m => GameStateRead m where
    readGameState :: m GameState


updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    gs <- readGameState
    inputs <- readInputState
    case inputStateDirection inputs of
        Nothing -> return $ gs { gameStateUserMovement = Nothing }
        Just dir -> return $ updatePlayer cfgs gs dir


updatePlayer :: Configs -> GameState -> Direction -> GameState
updatePlayer cfgs gs dir
    | gameStateUserMovement gs == Just dir = gs
    | otherwise = gs { gameStateUserState = (x'', y''), gameStateUserMovement = Just dir }
    where
        (xMove, yMove) = updatePosition dir
        (xMax, yMax) = configsBoardSize cfgs
        (x, y) = gameStateUserState gs
        x' = x + xMove
        y' = y + yMove
        x'' = if x' < xMax  && x' >= 0 then x' else x
        y'' = if y' < yMax && y' >= 0 then y' else y

updatePosition :: Direction -> (Int, Int)
updatePosition DUp = (0, -1)
updatePosition DDown = (0, 1)
updatePosition DLeft = (-1, 0)
updatePosition DRight = (1, 0)
