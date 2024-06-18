module GameState.Item
    ( pickupOnCollision
    , portalOnCollision
    , newArea
    ) where

import InputState
import GameState.Types
import GameState.Collision.BoundBox
import GameState.Collision.RTree

import Data.Unique 
import qualified Data.Map.Strict as M

onCollision :: GameArea -> InputState -> (BoundBox, Unique) -> GameArea
onCollision area inputs (ahb, collId) =
    case items M.! collId of
        CollectItem is -> pickupOnCollision area
        PortalItem p -> portalOnCollision area 

    where
        itemMan = gameStateItemManager area
        items = itemMap itemMan
        sP = spacePressed inputs

pickupOnCollision :: GameArea -> Bool -> (BoundBox, Unique) -> GameArea
pickupOnCollision area sP (ahb, collId) = area'
    where
        itemState = items M.! collId
        cm = collisionMap area
        p = gameStatePlayer area
        items' = M.adjust (const itemState {itemPosition=Nothing}) collId items
        cm' = if sP then delete ahb cm else cm
        pState = playerState p
        player' = p { playerState = pState { playerItems = M.insertWith (+) (itemInfo itemState) 1 (playerItems pState) }}
        iM' = if sP then itemMan { itemMap = items' } else itemMan { itemHighlighted = Just collId}
        area' = if sP then area { gameStateItemManager = iM', collisionMap = cm', gameStatePlayer = player' } else area { gameStateItemManager = iM' }


portalOnCollision :: GameArea -> Bool -> ItemManager -> CollectionEntry -> Portal -> GameArea
portalOnCollision area sP im portal collId = area'
    where
        itemMan = gameStateItemManager area
        items = itemMap itemM
        im' = im { itemHighlighted = Just collId }
        area' = if sP then newArea area (_portalArea portalState) else area { gameStatePortals = pm' } 


newArea :: GameArea -> AreaLocation -> GameArea
newArea area Inside = trace "move to new area" area

