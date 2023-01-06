{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState
import GameState.Types
import OutputHandles.Types
import GameState.Collision

import qualified Data.Text as T
import qualified SDL

import Utils

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class
import Data.Unique

import Debug.Trace

instance Show Unique where
    show = show . hashUnique

-- bad literals in code
initPlayer :: OutputHandles -> Player
initPlayer outs = Player textureEntry (startX, startY) (Left DDown) mempty
    where
        textureEntry = textures outs ! "character"
        startX = 0
        startY = 0

initItems :: OutputHandles -> Background -> IO (ItemManager, ObjectMap, CollisionMap Unique)
initItems outs back = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight iW iH
    insertItems (Item mushroomEntry Mushroom) itemPos
    where
        mushroomEntry = textures outs ! "mushroom"
        backT = area back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 25
        maxItems = 50
        iW = textureWidth mushroomEntry
        iH = textureHeight mushroomEntry

insertItems :: Item -> [(Int, Int)] -> IO (ItemManager, ObjectMap, CollisionMap Unique)
insertItems item positions = foldM (\maps (x, y) -> insertItem item (x, y) maps) (mempty, mempty, mempty) positions

insertItem :: Item -> (Int, Int) -> (ItemManager, ObjectMap, CollisionMap Unique) -> IO (ItemManager, ObjectMap, CollisionMap Unique)
insertItem item (x, y) (im, m, cm) = do
    un <- newUnique
    let m' = M.insert un BoardItem m
        im' = M.insert un (ItemState item (Just (x, y))) im
        t = itemTexture item
        cm' = insertCollision (x,y,textureWidth t,textureHeight t,un) cm
    return (im', m', cm')



-- bad literals in code
initBackground :: OutputHandles -> Background
initBackground outs = Background ((textures outs) ! "outside") 0 0


initGameState :: Configs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    let back = initBackground outs
    (im, m, cm) <- initItems outs back
    return $ GameState back (initPlayer outs) im m cm World

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)


stopMoveDirection :: Player -> Player
stopMoveDirection player = case playerMovement player of
    Left d -> player
    Right (d, _, _) -> player { playerMovement = Left d }

updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    gs <- readGameState
    inputs <- readInputState
    (moved, player') <- case inputStateDirection inputs of
        Nothing -> return (False, stopMoveDirection $ gameStatePlayer gs)
        Just dir -> return (True, updatePlayer (background gs) (gameStatePlayer gs) dir)
    let background' = updateBackground cfgs (background gs) player'
        gs' = (gs { background = background', gameStatePlayer = player' })
    return $ if moved then collisionCheck gs' else gs'


updateBackground :: Configs -> Background -> Player -> Background
updateBackground cfgs back player = back { xOffset = getOffset playerX windowX xMax
                                         , yOffset = getOffset playerY windowY yMax
                                         }
    where
        windowX = boardSizeX cfgs
        windowY = boardSizeY cfgs
        xMax = textureWidth $ area back
        yMax = textureHeight $ area back
        (playerX, playerY) = playerPosition $ player
        getOffset player window areaMax
            | player < (div window 2) = 0
            | player > areaMax - (div window 2) = areaMax - window
            | otherwise = player - (div window 2)

collisionCheck :: GameState -> GameState
collisionCheck gs =
    case detectCollision (playerX,playerY,playerWidth,playerHeight) cm of
        [] -> gs
        collisions ->
            let (items', cm', player') = foldl updateObject (items, cm, player) collisions
            in gs { gameStatePlayer = player', gameStateItemManager = items', collisionMap = cm' }
    where
        cm = collisionMap gs
        items = gameStateItemManager gs
        player = gameStatePlayer gs
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        playerHeight = textureHeight playerT
        (playerX, playerY) = playerPosition $ player
        updateObject (items, cm, player) a =
            let item = items ! a
                (x, y, w, h) = getItemDimensions item
                items' = M.adjust (\_ -> item {itemPosition=Nothing}) a items
                cm' = deleteCollision (x, y, w, h, a) cm
                player' = player { playerItems = (M.insertWith (+) (itemType (itemInfo item)) 1 (playerItems player)) }
            in (items', cm', player')


updatePlayer :: Background -> Player -> Direction -> Player
updatePlayer back player@(Player cfg pos (Right (d, l, f)) items) newDir
    | d == newDir && l >= 6 = player {playerPosition =
        newPosition back player newDir, playerMovement = Right (newDir, 0, mod (f + 1) 8 ) }
    | d == newDir = player {playerMovement = Right (d, l + 1, f)}
    | otherwise = player { playerPosition = newPosition back player newDir, playerMovement = Right (newDir, 0, 0) }
updatePlayer back player dir = player { playerPosition = newPosition back player dir, playerMovement = Right (dir, 0, 0) }


newPosition :: Background -> Player -> Direction -> (Int, Int)
newPosition back player dir = (x'', y'')
    where
        charSizeX = textureWidth (playerTexture player)
        charSizeY = textureHeight (playerTexture player)
        (xMove, yMove) = updatePosition dir
        xMax = (textureWidth $ area back) - charSizeX
        yMax = (textureHeight $ area back) - charSizeY
        xMin = 0
        yMin = 0
        (x, y) = playerPosition player
        x' = x + xMove
        y' = y + yMove
        x'' = if x' < xMax  && x' >= xMin then x' else x
        y'' = if y' < yMax && y' >= yMin then y' else y


updatePosition :: Direction -> (Int, Int)
updatePosition DUp = (0, -5)
updatePosition DDown = (0, 5)
updatePosition DLeft = (-5, 0)
updatePosition DRight = (5, 0)
