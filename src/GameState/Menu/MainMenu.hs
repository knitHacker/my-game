{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.MainMenu
    ( initMainMenu
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types
    ( Color(White)
    , OutputHandles(textures)
    , TextDisplay(TextDisplay)
    )
import GameState.Types
    ( MenuCursor(MenuCursor)
    , Menu(Menu)
    , MenuAction(GameExit, GameStart)
    )

initMainMenu :: OutputHandles -> Menu
initMainMenu outs = Menu words opts (MenuCursor 0 arrowEntry)
    where
        arrowEntry = textures outs M.! "green_arrow"
        words = [ TextDisplay "My Game" 10 10 175 100 White
                , TextDisplay "Press ENTER to select" 75 150 100 20 White
                ]
        opts = [GameStart, GameExit]
