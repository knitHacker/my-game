module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , GameMode(..)
    , Player(..)
    , ItemManager
    , Item(..)
    , ItemType(..)
    , ItemState(..)
    , Background(..)
    , BoardObject(..)
    , ObjectMap
    , getItemDimensions
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

type ObjectMap = M.Map Unique BoardObject


data GameState = GameState
    { background :: Background
    , gameStatePlayer :: Player
    , gameStateItemManager :: ItemManager
    , collisionObjects :: ObjectMap
    , collisionMap :: CollisionMap Unique
    , gameStateMode :: GameMode
    }


data BoardObject =
      BoardItem
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


data ItemType = Mushroom deriving (Show, Eq, Ord)

getItemDimensions :: ItemState -> (Int, Int, Int, Int)
getItemDimensions (ItemState item pos) =
    case pos of
        Just (x, y) -> (x, y, textureWidth t, textureHeight t)
        Nothing -> (0, 0, textureWidth t, textureHeight t)
    where
        t = itemTexture item

data Item = Item
    { itemTexture :: TextureEntry
    , itemType :: ItemType
    }

data ItemState = ItemState
    { itemInfo :: Item
    , itemPosition :: Maybe (Int, Int)
    }

type ItemManager = M.Map Unique ItemState


data Background = Background
    { area :: TextureEntry
    , xOffset :: Int
    , yOffset :: Int
    }
