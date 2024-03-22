module GameState.Item
    ( pickupOnCollision
    ) where

import InputState
import GameState.Types
import GameState.Collision.BoundBox
import GameState.Collision.RTree

import Data.Unique 
import qualified Data.Map.Strict as M

pickupOnCollision :: GameArea -> InputState -> (BoundBox, Unique) -> GameArea
pickupOnCollision area inputs (ahb, collId) = area'
    where
        itemMan = gameStateItemManager area
        items = itemMap itemMan
        itemState = items M.! collId
        cm = collisionMap area
        p = gameStatePlayer area
        sP = spacePressed inputs
        items' = M.adjust (const itemState {itemPosition=Nothing}) collId items
        cm' = if sP then delete ahb cm else cm
        pState = playerState p
        player' = p { playerState = pState { playerItems = M.insertWith (+) (itemInfo itemState) 1 (playerItems pState) }}
        iM' = if sP then itemMan { itemMap = items' } else itemMan { itemHighlighted = Just collId}
        area' = if sP then area { gameStateItemManager = iM', collisionMap = cm', gameStatePlayer = player' } else area { gameStateItemManager = iM' }