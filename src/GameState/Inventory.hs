{-# LANGUAGE OverloadedStrings #-}
module GameState.Inventory
    ( initInventory
    , updateGameInventory
    ) where

import qualified Data.Map.Strict as M

import GameState.Types
import OutputHandles.Types
import InputState

initInventory :: OutputHandles -> GameArea -> Inventory
initInventory outs area = Inventory area (MenuCursor 0 arrowEntry) bagEntry
    where
        arrowEntry = textures outs M.! "green_arrow"
        bagEntry = textures outs M.! "bag"

updateGameInventory :: InputState -> Inventory -> GameState
updateGameInventory inputs inv
    | escapeJustPressed inputs = GameStateArea (areaInfo inv) True
    | otherwise = GameInventory inv
