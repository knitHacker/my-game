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


-- bad literals in code
initPlayer :: OutputHandles -> Player
initPlayer outs = Player textureEntry (startX, startY) (Left DDown) mempty
    where
        textureEntry = textures outs ! "character"
        startX = div (textureWidth textureEntry) 2
        startY = div (textureHeight textureEntry) 2

initItems :: OutputHandles -> Background -> IO ItemManager
initItems outs back = do
    numberOfItems <- randomValue minItems maxItems
    itemPos <- replicateM numberOfItems $ randomPosition boardWidth boardHeight iW iH
    return $ ItemManager $ M.fromList $ zip itemPos $ repeat $ Item mushroomEntry Mushroom
    where
        mushroomEntry = textures outs ! "mushroom"
        backT = area back
        boardWidth = textureWidth backT
        boardHeight = textureHeight backT
        boardSize = boardWidth * boardHeight
        minItems = 10
        maxItems = 50
        iW = div (textureWidth mushroomEntry) 2
        iH = div (textureHeight mushroomEntry) 2


-- bad literals in code
initBackground :: OutputHandles -> Background
initBackground outs = Background ((textures outs) ! "outside") 0 0


initGameState :: Configs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    let back = initBackground outs
    items <- initItems outs back
    return $ GameState back (initPlayer outs) items World mempty


insertItems :: ItemManager -> IO (CollisionMap Unique, M.Map Unique BoardObject)
insertItems im = undefined
    
    where
        items = gameItems im

insertItem :: Item -> (CollisionMap Unique, M.Map Unique BoardObject) -> IO (CollisionMap Unique, M.Map Unique BoardObject)
insertItem item (cm, m) = do
    un <- newUnique
    let m' = M.insert un item m
        cm' = insert
    undefined

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue iW (width - iW)
    yPos <- randomValue iH (height - iH)
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
    player' <- case inputStateDirection inputs of
        Nothing -> return $ stopMoveDirection $ gameStatePlayer gs
        Just dir -> return $ updatePlayer (background gs) (gameStatePlayer gs) dir
    let background' = updateBackground cfgs (background gs) player'
    return $ collisionCheck (gs { background = background', gameStatePlayer = player' })


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
    case M.lookup playerPos items of
        Nothing -> gs
        Just item ->
            let items' = M.delete playerPos items
                player' = player { playerItems = (M.insertWith (+) (itemType item) 1 (playerItems player)) }
            in gs { gameStatePlayer = player', gameStateItemManager = (ItemManager items') }
    where
        items = gameItems $ gameStateItemManager gs
        player = gameStatePlayer gs
        playerT = playerTexture player
        playerWdith = textureWidth playerT
        playerHeight = textureHeight playerT
        playerPos = playerPosition $ player


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
        charSizeX = div (textureWidth (playerTexture player)) 2
        charSizeY = div (textureHeight (playerTexture player)) 2
        (xMove, yMove) = updatePosition dir
        xMax = (textureWidth $ area back) - charSizeX
        yMax = (textureHeight $ area back) - charSizeY
        xMin = charSizeX
        yMin = charSizeY
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
