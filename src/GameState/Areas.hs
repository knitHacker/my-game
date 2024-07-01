module GameState.Areas
    ( updateArea
    ) where

import Data.Word (Word32)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Maybe

import Configs
    ( CharacterMovement(..)
    , GameConfigs(..)
    )
import InputState
    ( escapeJustPressed
    , iPressed
    , spacePressed
    , Direction(..)
    , InputState(..)
    )
import OutputHandles.Types
    ( OutputHandles
    , TextureEntry(..)
    )
import GameState.Collision.BoundBox
    ( union
    , BoundBox(BB)
    )
import GameState.Collision.RTree
    ( delete
    , getCollisionBB
    , RTree
    )
import GameState.Player
    ( getBoundBox
    , getDirection
    , getPlayerHitbox
    , getPlayerPickupBox
    , movePlayer
    , playerMove
    )
import GameState.Types
    ( Background(..)
    , Item(..)
    , ItemState(..)
    , Player(..)
    , PlayerState(..)
    , PlayerConfig(..)
    , NPCManager(..)
    , GameArea(..)
    , PlayerMovement(..)
    , PlayerAction(..)
    , GameState(..)
    , ItemManager(..)
    , Barriers
    , AreaLocation(..)
    )
import GameState.Menu.PauseMenu ( initPauseMenu )
import GameState.Inventory ( initInventory )
import GameState.Item
import GameState.Areas.Inside
import GameState.Background

import Debug.Trace

updateArea :: OutputHandles -> GameConfigs -> InputState -> GameArea -> Either (AreaLocation, Player) GameState
updateArea outs cfgs inputs area
    | escapeJustPressed inputs = Right $ GameMenu (initPauseMenu outs area) True
    | iPressed inputs = Right $ GameInventory (initInventory outs area)
    | otherwise =
        case playerM of
            Nothing -> updateArea' inputs area cfgs (Left player) (npcNext player)
            Just p -> updateArea' inputs area cfgs (Right p) (npcNext p)
    where
        player = gameStatePlayer area
        npc = gameStateNPCs area
        back = background area
        rt = barrierCollisions area
        ts = inputTimestamp inputs
        playerM = updatePlayer (barrierCollisions area) back inputs player
        npcNext p = updateNPC ts rt back p npc

updateArea' :: InputState -> GameArea -> GameConfigs -> Either Player Player -> Maybe NPCManager -> Either (AreaLocation, Player) GameState
updateArea' inputs area cfgs pM nM =
    case (pM, nM) of
        (Left p, Nothing) ->
            let aE = areaColl area p
            in convertE aE
        (Left p, Just n') ->
            let a = areaNPC n'
                aE = areaColl a p
            in convertE aE
        (Right p', Nothing) ->
            let a = areaPlay p'
                aE = areaColl a p'
                aE' = backgroundNew aE p'
            in convertE aE'
        (Right p', Just n') ->
            let a = areaBoth p' n'
                aE = areaColl a p'
                aE' = backgroundNew aE p'
            in convertE aE'
    where
        sP = spacePressed inputs
        areaNPC npc' = (area { gameStateNPCs = npc' })
        areaPlay player' = (area { gameStatePlayer = player' })
        areaBoth player' npc' = (area { gameStatePlayer = player', gameStateNPCs = npc' })
        areaColl a p = collisionActionCheck a p inputs
        backgroundNew l@(Left _) _ = l
        backgroundNew (Right a) p = Right $ (\b -> a { background = b}) $ updateBackground cfgs (background a) p
        convertE (Left info) = Left info
        convertE (Right a) = Right $ GameStateArea a True


updatePlayer :: Barriers -> Background -> InputState -> Player -> Maybe Player
updatePlayer rtree back inputs player@(Player cfgs state) =
    case (inputStateDirection inputs, playerAction state) of
        (Nothing, PlayerStanding _ _) -> Nothing
        (Nothing, PlayerMoving (PlayerMove d _ _)) -> Just (player {playerState = state { playerAction = PlayerStanding d ts}})
        (Just iDir, PlayerStanding _ _) -> Just (movePlayer rtree player iDir ts 0 (newPos iDir))
        (Just iDir, PlayerMoving pm@(PlayerMove oldDir oldTs f))
            | iDir == oldDir && (ts - oldTs) > rate -> Just (movePlayer rtree player iDir ts (mod (f + 1) 8) (newPos iDir))
            | iDir == oldDir -> Nothing
            | otherwise -> Just (movePlayer rtree player iDir ts 0 (newPos iDir))
    where
        ts = inputTimestamp inputs
        newPos = newCharPosition back player
        rate = stepRate $ playerMoveCfgs cfgs


newCharPosition :: Background -> Player -> Direction -> (Int, Int)
newCharPosition back player dir = (x'', y'')
    where
        charSizeX = textureWidth $ playerTexture $ playerCfgs player
        charSizeY = textureHeight $ playerTexture $ playerCfgs player
        moveAmt = moveStep $ playerMoveCfgs $ playerCfgs player
        (xMove, yMove) = updatePosition moveAmt dir
        xMax = textureWidth (backArea back) - charSizeX
        yMax = textureHeight (backArea back) - charSizeY
        xMin = 0
        yMin = 0
        (x, y) = playerPosition $ playerState player
        x' = x + xMove
        y' = y + yMove
        x'' = max (min x' xMax) xMin
        y'' = max (min y' yMax) yMin


updatePosition :: Int -> Direction -> (Int, Int)
updatePosition m DUp = (0, -m)
updatePosition m DDown = (0, m)
updatePosition m DLeft = (-m, 0)
updatePosition m DRight = (m, 0)


updateNPC :: Word32 -> Barriers -> Background -> Player -> NPCManager -> Maybe NPCManager
updateNPC ts rtree back player (NPCManager p) =
    case followPlayer ts rtree back player p of
        Nothing -> Nothing
        Just p' -> Just (NPCManager p')


-- TODO: Add follow collision check (how not to get stuck)
followPlayer :: Word32 -> Barriers -> Background -> Player -> Player -> Maybe Player
followPlayer ts rtree back player p@(Player cfgs state) =
    case (targetM, playerAction state) of
        (Nothing, PlayerMoving pm@(PlayerMove oldDir oldTs _))
            | (ts - oldTs) > rate -> Just (p {playerState = state {playerAction = PlayerStanding oldDir ts}})
            | otherwise -> Nothing
        (Nothing, _) -> Nothing
        (Just (dir, pos), PlayerStanding _ oldTs) ->
            if ts - oldTs > rate then Just $ updateFollow pos dir 0 else Nothing
        (Just (dir, pos), PlayerMoving pm@(PlayerMove oldDir oldTs f))
            | dir == oldDir && (ts - oldTs) > rate -> Just $ updateFollow pos dir (mod (f + 1) 8)
            | dir == oldDir -> Nothing
            | (ts - oldTs) > rate -> Just $ updateFollow pos dir 0
            | otherwise -> Nothing
    where
        rate = stepRate $ playerMoveCfgs cfgs
        targetM = followTarget rtree back player p
        updateFollow pos dir f = -- movePlayer back p dir ts f pos
            let
                movement = PlayerMoving (PlayerMove dir ts f)
            in p {playerState = state {playerPosition = pos, playerAction = movement}}


followTarget :: Barriers -> Background -> Player -> Player -> Maybe (Direction, (Int, Int))
followTarget rtree back player follow
    -- already in the target location but facing a different direction than the player so change facing direction
    | leftDiff' == 0 && upDiff' == 0 && dir /= nDir = Just (dir, (folX, folY))
    -- already in the target location so don't move
    | leftDiff' == 0 && upDiff'== 0 = Nothing
    -- left direction is furthest from the target so move left
    | leftDiff' >= rightDiff' && leftDiff' >= downDiff' && leftDiff' >= upDiff' = Just (DLeft, leftMove)
    -- right direction is furthest from the target so move right
    | rightDiff' >= upDiff' && rightDiff' >= downDiff' = Just (DRight, rightMove)
    -- up direction is furthest from the target so move up
    | upDiff' >= downDiff' = Just (DUp, upMove)
    -- only other option is down direction is furthest from the target so move down
    | otherwise = Just (DDown, downMove)
    where
        leftDiff = folX - targetX'
        rightDiff = targetX' - folX
        upDiff = folY - targetY'
        downDiff = targetY' - folY
        (_, leftMove) = playerMove rtree follow DLeft leftDiff
        leftDiff' = folX - fst leftMove
        (_, rightMove) = playerMove rtree follow DRight rightDiff
        rightDiff' = fst rightMove - folX
        (_, upMove) = playerMove rtree follow DUp upDiff
        upDiff' = folY - snd upMove
        (_, downMove) = playerMove rtree follow DDown downDiff
        downDiff' = snd downMove - folY
        (folX, folY) = playerPosition $ playerState follow
        (BB folXLeft folYUp folXRight folYDown) = getBoundBox dir $ playerHitBoxes $ playerCfgs follow
        dir = getDirection player
        nDir = getDirection follow
        (pX, pY) = playerPosition $ playerState player
        (BB xLeft yUp xRight yDown)  = getBoundBox dir $ playerHitBoxes $ playerCfgs player
        xMax = textureWidth $ backArea back
        yMax = textureHeight $ backArea back
        (targetX, targetY) = case dir of
                                DUp -> (pX - folXLeft + xLeft, pY + yDown + 15)
                                DDown -> (pX - folXLeft, pY - 15)
                                DLeft -> (pX + xRight + 15, pY + yDown - folYDown)
                                DRight -> (pX - 15 - folXRight, pY + yDown - folYDown)
        (targetX', targetY') = (min (xMax - folXRight) (max (-folXLeft) targetX)
                               , min (yMax - folYDown) (max 0 targetY))


collisionActionCheck :: GameArea -> Player -> InputState -> Either (AreaLocation, Player) GameArea
collisionActionCheck gs player inputs =
    case getCollisionBB hb' cm of
        [] -> Right $ gs { gameStatePlayer = player, gameStateItemManager = items {itemHighlighted = Nothing} }
        collisions ->
            let (u, itemId) = head collisions -- TODO: most overlapped one instead of first?
                itemState = itemMap items ! itemId
            in onCollision gs inputs (u, itemId)
    where
        oldPlayer = gameStatePlayer gs
        oldHb = getPlayerPickupBox oldPlayer
        hb = getPlayerPickupBox player
        hb' = oldHb `union` hb
        items = gameStateItemManager gs
        cm = collisionMap items

resetItems :: GameArea -> GameArea
resetItems area
    | isJust $ itemHighlighted $ gameStateItemManager area = undefined
    | otherwise = area
