module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , GameMode(..)
    , Player(..)
    , ItemManager(..)
    , Item(..)
    , ItemType(..)
    , Background(..)
    , BoardObject(..)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Unique

import Configs
import InputState
import OutputHandles.Types
import GameState.Collision

import Utils




data GameState = GameState
    { background :: Background
    , gameStatePlayer :: Player
    , gameStateItemManager :: ItemManager
    , gameStateMode :: GameMode
    , collisionMap :: CollisionMap Unique
    , collisionObjects :: M.Map Unique BoardObject
    }


data BoardObject =
      BoardItem Item
    | BoardPlayer Player

data GameMode =
      Menu
    | Inventory
    | World deriving (Show, Eq)

class Monad m => GameStateRead m where
    readGameState :: m GameState


data Player = Player
    { playerTexture :: TextureEntry
    , playerPosition :: (Int, Int)
    , playerMovement :: Either Direction (Direction, Int, Int)
    , playerItems :: M.Map ItemType Int
    }


data ItemManager = ItemManager
    { gameItems :: M.Map (Int, Int) Item
    }


data ItemType = Mushroom deriving (Show, Eq, Ord)


data Item = Item
    { itemTexture :: TextureEntry
    , itemType :: ItemType
    }

data Background = Background
    { area :: TextureEntry
    , xOffset :: Int
    , yOffset :: Int
    }
