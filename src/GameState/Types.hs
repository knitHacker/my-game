module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , GameArea(..)
    , Menu(..)
    , MenuAction(..)
    , MenuCursor(..)
    , Player(..)
    , PlayerAction(..)
    , PlayerState(..)
    , PlayerConfig(..)
    , PlayerMovement(..)
    , ItemManager
    , Item(..)
    , ItemState(..)
    , Background(..)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Unique
import Data.Word
import qualified Data.Text as T

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

data PlayerAction =
    PlayerStanding Direction
    | PlayerMoving PlayerMovement
    deriving (Show, Eq)

data PlayerMovement = PlayerMove
    { playerDirection :: Direction
    , lastMoveTimestamp :: Word32
    , animationFrame :: Int
    } deriving (Show, Eq)

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
    , collisionMap :: RTree Unique -- TODO: move this into ItemManager probably
    }


class Monad m => GameStateRead m where
    readGameState :: m GameState

data PlayerConfig = PlayerCfg
    { playerTexture :: TextureEntry
    , playerHitBoxes :: CharacterHitBoxes
    , playerMoveCfgs :: CharacterMovement
    }

data PlayerState = PlayerState
    { playerPosition :: (Int, Int)
    , playerAction :: PlayerAction
    , playerItems :: M.Map T.Text Int
    }

data Player = Player
    { playerCfgs :: PlayerConfig
    , playerState :: PlayerState
    }


data Item = Item
    { itemTexture :: TextureEntry
    , itemHb :: BoundBox
    , itemType :: T.Text
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
