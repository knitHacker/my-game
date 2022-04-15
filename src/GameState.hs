module GameState
    ( GameState(..)
    , initGameState
    , GameStateRead(..)
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState


import Control.Monad.IO.Class

data GameState = GameState
    { gameStateUserState :: (Int, Int)
    , gameStateUserMovement :: Maybe Direction
    } deriving (Show, Eq)


initGameState :: IO GameState
initGameState = return $ GameState (0,0) Nothing


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
