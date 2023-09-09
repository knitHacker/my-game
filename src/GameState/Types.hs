module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , GameArea(..)
    , Menu(..)
    , MenuAction(..)
    , MenuCursor(..)
    , Player(..)
    , ItemManager
    , Item(..)
    , ItemType(..)
    , ItemState(..)
    , Background(..)
    , BoardObject(..)
    , ObjectMap
    , getItemDimensions
    , getDirection
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
    | GameMenu Menu
    | GameStateArea GameArea
    | GameExiting


data MenuAction =
    GameStart
    | GameExit
    | GameContinue GameArea
    | GameStartMenu

data Menu = Menu
    { texts :: [TextDisplay]
    , options :: [MenuAction]
    , cursor :: MenuCursor
    }

data MenuCursor = MenuCursor
    { cursorPos :: Int
    , cursorTexture :: TextureEntry
    }

data GameArea = GameArea
    { background :: Background
    , gameStatePlayer :: Player
    , gameStateItemManager :: ItemManager
    , collisionObjects :: ObjectMap
    , collisionMap :: CollisionMap Unique
    }


data BoardObject =
      BoardItem
    | BoardBarrier


class Monad m => GameStateRead m where
    readGameState :: m GameState


getDirection :: Player -> Direction
getDirection p = case playerMovement p of
    Left d -> d
    Right (d, _, _) -> d

data Player = Player
    { playerTexture :: TextureEntry
    , playerHitBox :: HitBox
    , playerPosition :: (Int, Int)
    , playerMovement :: Either Direction (Direction, Int, Int)
    , playerItems :: M.Map ItemType Int
    , playerCfgs :: CharacterMovement
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

instance Show Item where
    show = show . itemType

data ItemState = ItemState
    { itemInfo :: Item
    , itemPosition :: Maybe (Int, Int)
    }

type ItemManager = M.Map Unique ItemState


data Background = Background
    { area :: TextureEntry
    , xOffset :: Int
    , yOffset :: Int
    , backBarriers :: M.Map Unique ((Int, Int), TextureEntry)
    }
