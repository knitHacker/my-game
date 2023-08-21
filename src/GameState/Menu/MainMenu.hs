{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.MainMenu
    ( initMainMenu
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types
import GameState.Types

initMainMenu :: Menu
initMainMenu = Menu words opts 0 MainMenu
    where
        words = [ TextDisplay "My Game" 10 10 175 100 White
                , TextDisplay "Press ENTER to start" 75 150 100 20 White
                ]
        opts = [ ("Start", GameStart), ("Exit", GameExit)]
