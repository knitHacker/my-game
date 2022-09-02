module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , GameMode(..)
    , Player(..)
    , ItemManager(..)
    , Item(..)
    , ItemType(..)
    , Background(..)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M

import Configs
import InputState
import OutputHandles.Types

import Utils



data GameState = GameState
    { background :: Background
    , gameStatePlayer :: Player
    , gameStateItemManager :: ItemManager
    , gameStateMode :: GameMode
    }


data GameMode = Menu | Inventory | World deriving (Show, Eq)

class Monad m => GameStateRead m where
    readGameState :: m GameState


data Player = Player
    { playerTexture :: TextureEntry
    , playerPosition :: (Int, Int)
    , playerMovement :: Maybe (Direction, Int)
    , playerItems :: M.Map Item Int
    }


data ItemManager = ItemManager
    { gameItems :: M.Map (Int, Int) Item
    }


data ItemType = Blob deriving (Show, Eq, Ord)


data Item = Item
    { itemType :: ItemType
    } deriving (Show, Eq, Ord)

data Background = Background
    { area :: TextureEntry
    , xOffset :: Int
    , yOffset :: Int
    }
