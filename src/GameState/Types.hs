module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , GameArea(..)
    , Menu(..)
    , MenuAction(..)
    , MenuCursor(..)
    , Player(..)
    , PlayerMovement(..)
    , ItemManager
    , Item(..)
    , ItemType(..)
    , ItemState(..)
    , Background(..)
    , getItemDimensions
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Unique
import Data.Word

import Configs
import InputState
import OutputHandles.Types
import GameState.Collision.RTree
import GameState.Collision.BoundBox

import Utils


data GameState =
    GameMenu Menu Bool
    | GameStateArea GameArea Bool
    | GameExiting


data MenuAction =
    GameStart
    | GameExit
    | GameContinue GameArea
    | GameStartMenu

data PlayerMovement = PlayerMove
    { playerDirection :: Direction
    , lastMoveTimestamp :: Word32
    , animationFrame :: Int
    }

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
    , collisionMap :: RTree Unique
    }


class Monad m => GameStateRead m where
    readGameState :: m GameState

data Player = Player
    { playerTexture :: TextureEntry
    , playerHitBox :: HitBox
    , playerPosition :: (Int, Int)
    , playerMovement :: Either Direction PlayerMovement
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
    { backArea :: TextureEntry
    , backXOffset :: Int
    , backYOffset :: Int
    , backBarriers :: M.Map Unique ((Int, Int), TextureEntry)
    , backCollisions :: RTree Unique
    }
