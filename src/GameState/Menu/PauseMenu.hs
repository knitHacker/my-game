{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.PauseMenu
    ( initPauseMenu
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types
    ( Color(White), OutputHandles(textures), TextDisplay(TextDisplay) )
import GameState.Types
    ( GameArea,
      MenuCursor(MenuCursor),
      Menu(Menu),
      MenuAction(GameExit, GameContinue, GameStartMenu) )

initPauseMenu :: OutputHandles -> GameArea -> Menu
initPauseMenu outs a = Menu words [GameContinue a , GameStartMenu, GameExit] (MenuCursor 0 arrowEntry)
    where
        arrowEntry = textures outs M.! "green_arrow"
        words = [ TextDisplay "Pause" 10 10 175 100 White
                , TextDisplay "Press ENTER to return" 75 150 100 20 White
                ]
