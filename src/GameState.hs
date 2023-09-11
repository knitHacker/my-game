{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , isGameExiting
    , updateGameState
    ) where

import Control.Monad
import Configs
import InputState
import GameState.Types
import GameState.Areas.Outside
import GameState.Menu.MainMenu
import GameState.Menu.PauseMenu
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

initGameState :: Configs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
--    area <- initOutsideArea cfgs outs
    return $ GameMenu (initMainMenu outs) True

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)


stopMoveDirection :: Player -> Player
stopMoveDirection player = case playerMovement player of
    Left d -> player
    Right (d, _, _) -> player { playerMovement = Left d }

isGameExiting :: GameState -> Bool
isGameExiting GameExiting = True
isGameExiting _ = False

updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    case gs of
        GameMenu m _ -> liftIO $ updateGameStateInMenu m cfgs inputs outs
        GameStateArea area _ -> return $ updateGameStateInArea outs cfgs inputs area
        _ -> return gs

incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ opts c@(MenuCursor p _))
    | p < length opts - 1 = m { cursor = c { cursorPos = p + 1 }}
    | otherwise = m


decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ c@(MenuCursor 0 _)) = m
decrementMenuCursor m@(Menu _ _ c@(MenuCursor p _)) = m { cursor = c { cursorPos = p - 1 }}

updateGameStateInMenu :: Menu -> Configs -> InputState -> OutputHandles -> IO GameState
updateGameStateInMenu m cfgs inputs outs =
    if inputStateEnter inputs && not (inputRepeat inputs)
        then case (options m !! curPos) of
            GameStart -> do
                area <- initOutsideArea cfgs outs
                return $ GameStateArea area True
            GameExit -> return GameExiting
            GameStartMenu -> return $ GameMenu (initMainMenu outs) True
            GameContinue a -> return $ GameStateArea a True
        else case (inputRepeat inputs, inputStateDirection inputs) of
            (False, Just DUp) -> return $ GameMenu (decrementMenuCursor m) True
            (False, Just DDown) -> return $ GameMenu (incrementMenuCursor m) True
            _ -> return (GameMenu m False)
    where
        curPos = cursorPos $ cursor m


updateGameStateInArea :: OutputHandles -> Configs -> InputState -> GameArea -> GameState
updateGameStateInArea outs _ (InputState _ _ _ True _) area = GameMenu (initPauseMenu outs area) True
updateGameStateInArea _ _ (InputState _ Nothing _ _ _) a@(GameArea _ (Player _ _ _ (Left _) _ _) _ _ _) =
    GameStateArea a False
updateGameStateInArea _ cfgs inputs area = GameStateArea (area'' { background = background' }) True
    where
        player = gameStatePlayer area
        (moved, player') = case (inputStateDirection inputs, playerMovement player) of
            (Nothing, Left _) -> (False, player)
            (Nothing, Right (d, _, _)) -> (False, player { playerMovement = Left d })
            (Just dir, _) -> (True, updatePlayer (background area) (gameStatePlayer area) dir)
        area' = (area { gameStatePlayer = player' })
        area'' = if moved then collisionCheck area player' else area'
        background' = updateBackground cfgs (background area'') player'


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

collisionCheck :: GameArea -> Player -> GameArea
collisionCheck gs player =
    case detectCollision (playerX, collideYStart, playerWidth, collideHeight') cm of
        [] -> gs { gameStatePlayer = player }
        collisions ->
            let newState = foldl updateObject (Right (items, cm, player)) collisions
                (items', cm', player') = case newState of
                    Left (items', cm', player') -> (items', cm', player')
                    Right (items', cm', player') -> (items', cm', player')
            in gs { gameStatePlayer = player', gameStateItemManager = items', collisionMap = cm' }
    where
        collideHeight' = collideHeight $ playerHitBox player
        collideYStart = playerY + playerHeight - collideHeight'
        cm = collisionMap gs
        items = gameStateItemManager gs
        om = collisionObjects gs
        oldPlayer = gameStatePlayer gs
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        playerHeight = textureHeight playerT
        (playerX, playerY) = playerPosition $ player
        updateObject (Right (items, cm, player)) a =
            case om ! a of
                BoardItem -> updateItem (items, cm, player) a
                BoardBarrier ->
                    let ((x, y), tE) = backBarriers (background gs) ! a
                    in Left (items, cm, fixPlayerPosition player x y (textureWidth tE) (textureHeight tE))
        updateObject lGS _ = lGS
        updateItem (items, cm, player) a =
            let item = items ! a
                (x, y, w, h) = getItemDimensions item
                items' = M.adjust (\_ -> item {itemPosition=Nothing}) a items
                cm' = deleteCollision (x, y, w, h, a) cm
                player' = player { playerItems = (M.insertWith (+) (itemType (itemInfo item)) 1 (playerItems player)) }
            in (Right (items', cm', player'))


fixPlayerPosition :: Player -> Int -> Int -> Int -> Int -> Player
fixPlayerPosition player x y w h =
    case getDirection player of
        DUp -> player { playerPosition = (playerX, playerY' - playerHeight) }
        DDown -> player { playerPosition = (playerX, playerY' - playerHeight) }
        DLeft -> player { playerPosition = (playerX', playerY) }
        DRight -> player {playerPosition = (playerX', playerY) }
    where
        playerX' = newPosition playerX (playerX + playerWidth) x (x + w) playerWidth True
        playerY' = newPosition (playerY + playerHeight - 4) (playerY + playerHeight) y (y + h) 4 False
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        playerHeight = textureHeight playerT
        (playerX, playerY) = playerPosition $ player
        newPosition p1 p2 b1 b2 width isX =
            case whichSide p1 p2 b1 b2 of
                LeftOf -> p1
                RightOf -> p1
                LeftOverlap -> if isX then b1 - 1 - width else b1 -  1
                RightOverlap -> if isX then b2 + 1 else b2 + 1 + width
                In -> if p1 - b1 < b2 - p2 then (if isX then b1 - 1 - width else b1 - 1) else (if isX then b2 + 1 else b2 + 1 + width)
                o -> p1
        whichSide playL playR l r = getOrder (playL, playR) (l, r)


updatePlayer :: Background -> Player -> Direction -> Player
updatePlayer back player@(Player cfg bb pos (Right (d, l, f)) items mCfg) newDir
    | d == newDir && l >= rate = player {playerPosition =
        newPosition back player newDir, playerMovement = Right (newDir, 0, mod (f + 1) 8 ) }
    | d == newDir = player {playerMovement = Right (d, l + 1, f)}
    | otherwise = player { playerPosition = newPosition back player newDir, playerMovement = Right (newDir, 0, 0) }
    where
        rate = stepRate mCfg
updatePlayer back player dir = player { playerPosition = newPosition back player dir, playerMovement = Right (dir, 0, 0) }


newPosition :: Background -> Player -> Direction -> (Int, Int)
newPosition back player dir = (x'', y'')
    where
        charSizeX = textureWidth (playerTexture player)
        charSizeY = textureHeight (playerTexture player)
        moveAmt = moveStep $ playerCfgs player
        (xMove, yMove) = updatePosition moveAmt dir
        xMax = (textureWidth $ area back) - charSizeX
        yMax = (textureHeight $ area back) - charSizeY
        xMin = 0
        yMin = 0
        (x, y) = playerPosition player
        x' = x + xMove
        y' = y + yMove
        x'' = max (min x' xMax) xMin
        y'' = max (min y' yMax) yMin


updatePosition :: Int -> Direction -> (Int, Int)
updatePosition m DUp = (0, -m)
updatePosition m DDown = (0, m)
updatePosition m DLeft = (-m, 0)
updatePosition m DRight = (m, 0)
