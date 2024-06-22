module GameState.Item
    ( onCollision
    ) where

import InputState
import GameState.Types
import GameState.Collision.BoundBox
import GameState.Collision.RTree

import Data.Unique
import qualified Data.Map.Strict as M

import Debug.Trace

onCollision :: GameArea -> InputState -> (BoundBox, CollisionEntry) -> GameArea
onCollision area inputs (ahb, collId) =
    case items M.! collId of
        CollectItem is -> pickupOnCollision area sP is (ahb, collId)
        PortalItem p -> portalOnCollision area sP itemMan p collId

    where
        itemMan = gameStateItemManager area
        items = itemMap itemMan
        sP = spacePressed inputs

pickupOnCollision :: GameArea -> Bool -> ItemState -> (BoundBox, Unique) -> GameArea
pickupOnCollision area sP itemState (ahb, collId) = area'
    where
        p = gameStatePlayer area
        itemMan = gameStateItemManager area
        items = itemMap itemMan
        cm = collisionMap itemMan
        items' = M.adjust (const (CollectItem itemState {itemPosition=Nothing})) collId items
        cm' = if sP then delete ahb cm else cm
        pState = playerState p
        player' = p { playerState = pState { playerItems = M.insertWith (+) (itemInfo itemState) 1 (playerItems pState) }}
        iM' = if sP then itemMan { itemMap = items', itemHighlighted = Nothing, collisionMap = cm' } else itemMan { itemHighlighted = Just collId}
        area' = if sP then area { gameStateItemManager = iM', gameStatePlayer = player' } else area { gameStateItemManager = iM' }


portalOnCollision :: GameArea -> Bool -> ItemManager -> Portal -> CollisionEntry -> GameArea
portalOnCollision area sP im port collId = area'
    where
        itemMan = gameStateItemManager area
        items = itemMap itemMan
        im' = im { itemHighlighted = Just collId }
        area' = if sP then newArea area (_portalArea port) else area { gameStateItemManager = im' }


newArea :: GameArea -> AreaLocation -> GameArea
newArea area InsideArea = trace "move to new area" area

