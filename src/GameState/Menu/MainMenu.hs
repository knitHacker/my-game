{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.MainMenu
    ( initMainMenu
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types
import GameState.Types

initMainMenu :: Menu
initMainMenu = Menu words Nothing MainMenu
    where
        words = M.fromList [ (0, TextDisplay "My Game" 10 10 175 100 White)
                           , (10, TextDisplay "Press ENTER to start" 75 150 100 20 White)
                           ]
