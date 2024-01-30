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
initInventory outs area = Inventory area bagEntry
    where
        bagEntry = textures outs M.! "bag"

updateGameInventory :: InputState -> Inventory -> GameState
updateGameInventory inputs inv
    | escapeJustPressed inputs = GameStateArea (areaInfo inv) True
    | otherwise = GameInventory inv
