{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.MainMenu
    ( initMainMenu
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types
import GameState.Types

initMainMenu :: Menu
initMainMenu = Menu words Nothing
    where
        words = M.fromList [ (0, TextDisplay "My Game" 10 10 100 50 White)
                           ]
