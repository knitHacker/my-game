module GameState.Portal
    ( portalOnCollision
    ) where

import GameState.Types
import InputState

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Unique

import Debug.Trace

portalOnCollision :: GameArea -> InputState -> Unique -> GameArea
portalOnCollision area inputs collId = area'
    where
        pm = gameStatePortals area
        portalState = pm ! collId
        sP = spacePressed inputs
        pm' = M.adjust (const portalState { portalDoorOpen = True }) collId pm
        area' = if sP then newArea area (portalArea portalState) else area { gameStatePortals = pm' } 


newArea :: GameArea -> AreaLocation -> GameArea
newArea area dest = trace "move to new area" area
