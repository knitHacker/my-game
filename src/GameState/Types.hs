module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , Player(..)
    , ItemManager(..)
    , Item(..)
    , ItemType(..)
    ) where

import Control.Monad
import Configs
import InputState

import Utils


import Control.Monad.IO.Class
import qualified Data.Map.Strict as M

data GameState = GameState
    { gameStatePlayer :: Player
    , gameStateItemManager :: ItemManager
    }

class Monad m => GameStateRead m where
    readGameState :: m GameState


data Player = Player
    { playerPosition :: (Int, Int)
    , playerMovement :: Maybe Direction
    , playerItems :: M.Map Item Int
    }


data ItemManager = ItemManager
    { gameItems :: M.Map (Int, Int) Item
    }


data ItemType = Blob deriving (Show, Eq, Ord)


data Item = Item
    { itemType :: ItemType
    } deriving (Show, Eq, Ord)
