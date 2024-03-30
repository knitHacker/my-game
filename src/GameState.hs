{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , isGameExiting
    , updateGameState
    ) where

import Control.Monad ()
import Configs ( ConfigsRead(readConfigs), GameConfigs )
import InputState
    ( enterJustPressed,
      Direction(DDown, DUp),
      InputRead(..),
      InputState(inputRepeat, inputStateDirection) )
import GameState.Types
    ( GameStateRead(..),
      GameArea,
      MenuCursor(MenuCursor, cursorPos),
      Menu(Menu, cursor, options),
      MenuAction(GameContinue, GameStart, GameExit, GameStartMenu),
      GameState(..) )
import GameState.Areas ( updateArea )
import GameState.Areas.Outside ( initOutsideArea )
import GameState.Menu.MainMenu ( initMainMenu )
import GameState.Inventory ( updateGameInventory )
import OutputHandles.Types ( OutputHandles, OutputRead(..) )

import qualified Data.Text as T
import qualified SDL

import Utils ( randomValue )

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )
import GameState.Player (initPlayer)

initGameState :: GameConfigs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    return $ GameMenu (initMainMenu outs) True

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)

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
        GameInventory inv -> return $ updateGameInventory inputs inv
        _ -> return gs

incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ opts c@(MenuCursor p _))
    | p < length opts - 1 = m { cursor = c { cursorPos = p + 1 }}
    | otherwise = m


decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ c@(MenuCursor 0 _)) = m
decrementMenuCursor m@(Menu _ _ c@(MenuCursor p _)) = m { cursor = c { cursorPos = p - 1 }}

updateGameStateInMenu :: Menu -> GameConfigs -> InputState -> OutputHandles -> IO GameState
updateGameStateInMenu m cfgs inputs outs =
    if enterJustPressed inputs
        then case options m !! curPos of
            GameStart -> do
                area <- initOutsideArea cfgs outs player
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
        player = initPlayer cfgs outs 0 0


updateGameStateInArea :: OutputHandles -> GameConfigs -> InputState -> GameArea -> GameState
updateGameStateInArea = updateArea
