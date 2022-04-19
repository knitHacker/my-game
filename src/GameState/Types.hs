module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
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

class Monad m => GameStateRead m where
    readGameState :: m GameState
