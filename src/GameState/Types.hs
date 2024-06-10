{-# LANGUAGE InstanceSigs #-}
module GameState.Types
    ( GameState(..)
    , Inventory(..)
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
    , ItemManager(..)
    , Item(..)
    , ItemState(..)
    , Background(..)
    , NPCManager(..)
    , AreaLocation(..)
    , CollisionType(..)
    ) where

import Control.Monad ()
import Control.Monad.IO.Class ()
import qualified Data.Map.Strict as M
import Data.Unique ( Unique, hashUnique )
import Data.Word ( Word32 )
import qualified Data.Text as T
import InputState

import Configs ( CharacterMovement, CharacterHitBoxes )
import InputState ( Direction )
import OutputHandles.Types ( TextureEntry, TextDisplay )
import GameState.Collision.RTree ( RTree )
import GameState.Collision.BoundBox ( BoundBox )

import Utils ()

data AreaLocation = Inside | Outside
    deriving (Eq, Ord, Show)

instance Show Unique where
    show:: Unique -> String
    show = show . hashUnique

-- Top level game state
--  Game menu is a menu with different options
--  Game state is where character walks around
--  Game exiting is how tell top loop to quit
data GameState =
    GameMenu Menu Bool
    | GameStateArea GameArea Bool
    | GameInventory Inventory
    | GameExiting

data Inventory = Inventory
    { areaInfo :: GameArea
    , bagTexture :: TextureEntry
    }

-- Actions that can be done from the Menu
--  Start makes a new game area
--  Exit quits the game
--  Continue returns to the game area already started
--  Start takes you to start menu (currently no saving)
data MenuAction =
    GameStart
    | GameExit
    | GameContinue GameArea
    | GameStartMenu

-- Actions that character in the game area can do
-- mostly for the animation
data PlayerAction =
    PlayerStanding Direction Word32
    | PlayerMoving PlayerMovement
    deriving (Show, Eq)

-- When character is moving in an area this is information
-- needed for correct moving amounts / animation
data PlayerMovement = PlayerMove
    { playerDirection :: Direction
    , lastMoveTimestamp :: Word32
    , animationFrame :: Int
    } deriving (Show, Eq)

-- Menu game state
--  Texts are the text to show including where to display
--  Options for actions from this menu
--  Cursor is the current option that is being pointed to
data Menu = Menu
    { texts :: [TextDisplay]
    , options :: [MenuAction]
    , cursor :: MenuCursor
    }

-- Menu cursor state
--  which option index the cursor is pointing to
--  the texture of the cursor
data MenuCursor = MenuCursor
    { cursorPos :: Int
    , cursorTexture :: TextureEntry
    }

data CollisionType = PortalCollision | ItemCollision
    deriving (Show, Eq, Ord)

-- Game area state
--  background state including objects you can't walk into
--  state of the player
--  state of the npc(s)
--  state of items
--  pickup item collision map
data GameArea = GameArea
    { background :: Background
    , gameStatePlayer :: Player
    , gameStateNPCs :: NPCManager
    , gameStateItemManager :: ItemManager
    , gameStatePortals :: M.Map Unique Portal
    , collisionMap :: RTree (CollisionType, Unique) -- TODO: move this into ItemManager probably
    }

-- NPC manager
--  Currently only the one follower NPC
data NPCManager = NPCManager
    { npcFollower :: Player
    }

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

-- The mostly static part of the player
--  The texture
--  The hitbox (in relation to the texture area)
--  The movement configs like how often to move and how far a step is
data PlayerConfig = PlayerCfg
    { playerTexture :: TextureEntry
    , playerHitBoxes :: CharacterHitBoxes
    , playerMoveCfgs :: CharacterMovement
    }

-- The changing part of the player state
--  position of the player in the background
--  what the player is doing
--  items the player is holding
data PlayerState = PlayerState
    { playerPosition :: (Int, Int)
    , playerAction :: PlayerAction
    , playerItems :: M.Map Item Int
    }

-- Player information
data Player = Player
    { playerCfgs :: PlayerConfig
    , playerState :: PlayerState
    }

-- Item information
--  The texture
--  The hitbox of the item
--  What the item is
data Item = Item
    { itemName :: T.Text
    , itemTexture :: TextureEntry
    , highlightTexture :: TextureEntry
    , itemHb :: BoundBox
    , itemType :: T.Text
    , itemOnCollision :: GameArea -> InputState -> (BoundBox, Unique) -> GameArea
    }

instance Eq Item where
    (==) :: Item -> Item -> Bool
    (==) i1 i2 = itemType i1 == itemType i2

instance Ord Item where
    compare :: Item -> Item -> Ordering
    compare i1 i2 = compare (itemType i1) (itemType i2)

instance Show Item where
    show :: Item -> String
    show = show . itemType

-- Item state
--  info of how to draw the item
--  position if it in the background
data ItemState = ItemState
    { itemInfo :: Item
    , itemPosition :: Maybe (Int, Int)
    }

-- Correspond items with a unique id
data ItemManager = ItemManager
    { itemMap :: M.Map Unique ItemState
    , itemHighlighted :: Maybe Unique
    }

data Portal = Portal
    { portalArea :: AreaLocation
    , portalDoorOpen :: Bool
    , portalClosedTexture :: TextureEntry
    , portalOpenTexture :: TextureEntry
    }

-- Background state
--  texture
--  offsets are the position of the window of the area shown
--  barriers are items the character can't walk through
--  map the barriers to a unique id and then the unque id is in the collisions
data Background = Background
    { backArea :: TextureEntry
    , backXOffset :: Int
    , backYOffset :: Int
    , backBarriers :: M.Map (Int, Int) TextureEntry
    , backCollisions :: RTree ()
    }
