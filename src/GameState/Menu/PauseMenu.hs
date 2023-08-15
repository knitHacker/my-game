{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.PauseMenu
    ( initPauseMenu
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types
import GameState.Types

initPauseMenu :: GameArea -> Menu
initPauseMenu a = Menu words Nothing $ PauseMenu a
    where
        words = M.fromList [ (0, TextDisplay "Pause" 10 10 175 100 White)
                           , (10, TextDisplay "Press ENTER to return" 75 150 100 20 White)
                           ]
