{-# LANGUAGE OverloadedStrings #-}

module GameState.Player
  ( playerStanding
  , getDirection
  ,  getBoundBox
  ,  getPlayerHitbox
  ,  getPlayerPickupBox
  ,  newPosition
  ,  movePlayer
  ,  playerMove
  ,  mainCharName
  , npcName
  , initNPC
  , initPlayer
  )
where

import qualified Data.Text as T
import Data.Map ((!))
import qualified Data.Map as M

import Configs
    ( CharacterMovement(moveStep),
      CharacterHitBoxes(sideHitbox, frontHitbox, pickupX, pickupY) )
import Data.Word (Word32)
import GameState.Collision.BoundBox
    ( BoundBox(BB), union, translate )
import GameState.Collision.RTree ( getIntersections )
import GameState.Types

import InputState ( Direction(..) )
import OutputHandles.Types ( TextureEntry(textureWidth) )
import Configs
import OutputHandles.Types

mainCharName :: T.Text
mainCharName = "main_character"

npcName :: T.Text
npcName = "dog"

initNPC :: GameConfigs -> OutputHandles -> Int -> Int -> NPCManager
initNPC cfgs outs startX startY = NPCManager $ Player playCfgs playState
    where
        playCfgs = PlayerCfg textureEntry hb cc
        playState = PlayerState (startX, startY) (PlayerStanding DDown 0) mempty
        charCfgs = characters cfgs ! npcName
        textureEntry = textures outs ! npcName
        hb = charHitBox charCfgs
        cc = charMovement charCfgs


initPlayer :: GameConfigs -> OutputHandles -> Int -> Int -> Player
initPlayer cfgs outs startX startY = Player playCfgs playState
    where
        playCfgs = PlayerCfg textureEntry hb cc
        playState = PlayerState (startX, startY) (PlayerStanding DDown 0) mempty
        charCfgs = characters cfgs ! mainCharName
        textureEntry = textures outs ! mainCharName
        hb = charHitBox charCfgs
        cc = charMovement charCfgs


playerStanding :: Player -> Bool
playerStanding player =
  case playerAction (playerState player) of
    PlayerStanding _ _ -> True
    _ -> False

newPosition :: Player -> Maybe Int -> Direction -> (Int, Int)
newPosition p@(Player cfg state) maxMoveM dir =
  case maxMoveM of
    Nothing -> newPosition' p step dir
    Just mm -> newPosition' p (min step mm) dir
  where
    step = moveStep $ playerMoveCfgs cfg

newPosition' :: Player -> Int -> Direction -> (Int, Int)
newPosition' p@(Player cfg state) mvAmt dir =
  case dir of
    DUp -> (x, y - mvAmt)
    DDown -> (x, y + mvAmt)
    DLeft -> (x - mvAmt, y)
    DRight -> (x + mvAmt, y)
  where
    (x, y) = playerPosition state

getDirection :: Player -> Direction
getDirection p = case playerAction (playerState p) of
  PlayerStanding d _ -> d
  PlayerMoving (PlayerMove d _ _) -> d

getPlayerHitbox :: Player -> BoundBox
getPlayerHitbox p@(Player cfg state) = translate x y hb
  where
    dir = getDirection p
    hb = getBoundBox dir $ playerHitBoxes cfg
    (x, y) = playerPosition state

getPlayerPickupBox :: Player -> BoundBox
getPlayerPickupBox p@(Player cfg state) = translate x y hb
  where
    dir = getDirection p
    pHB = playerHitBoxes cfg
    BB hx1 hy1 hx2 hy2 = getBoundBox dir pHB
    pickX = pickupX pHB
    pickY = pickupY pHB
    hb = case dir of
      DUp -> BB hx1 (hy1 - pickY) hx2 hy2
      DDown -> BB hx1 hy1 hx2 (hy2 + pickY)
      DLeft -> BB (hx1 - pickX) hy1 hx2 hy2
      DRight -> BB hx1 hy1 (hx2 + pickX) hy2
    (x, y) = playerPosition state

getBoundBox :: Direction -> CharacterHitBoxes -> BoundBox
getBoundBox dir hbs =
  case dir of
    DUp -> frontHb
    DDown -> frontHb
    DLeft -> sideHb
    DRight -> sideHb
  where
    sideHb = sideHitbox hbs
    frontHb = frontHitbox hbs

movePlayer :: Background -> Player -> Direction -> Word32 -> Int -> (Int, Int) -> Player
movePlayer back player@(Player cfg state) dir ts f (newX, newY) =
  player {playerState = state {playerAction = PlayerMoving (PlayerMove dir ts f), playerPosition = newPos}}
  where
    newPos = foldl movePlayer' (newX, newY) collisions
    (oldX, oldY) = playerPosition state
    hb = getBoundBox dir hitboxes
    hitboxes = playerHitBoxes cfg
    oldPlayerBB = translate oldX oldY hb
    rtree = backCollisions back
    collisions = getIntersections movementBB rtree
    playerT = playerTexture cfg
    playerWidth = textureWidth playerT
    playerBB = translate newX newY hb
    movementBB = oldPlayerBB `union` playerBB
    movePlayer' (x, y) b@(BB x1 y1 x2 y2) =
      case dir of
        DUp -> (x, y + (y2 - y1))
        DDown -> (x, y - (y2 - y1))
        DLeft -> (x + (x2 - x1), y)
        DRight -> (x - (x2 - x1), y)

playerMove :: Background -> Player -> Direction -> Int -> (Direction, (Int, Int))
playerMove back player@(Player cfg state) dir mvAmt = (dir, newPos)
  where
    (newX, newY) = newPosition player (Just mvAmt) dir
    -- end position after running into collisions
    newPos = foldl movePlayer' (newX, newY) collisions
    -- original position
    (oldX, oldY) = playerPosition state
    -- get hitbox for the directions moved in
    hb = getBoundBox dir hitboxes
    -- get hit boxes (not relative to position)
    hitboxes = playerHitBoxes cfg
    -- old position hit box
    oldPlayerBB = translate oldX oldY hb
    -- rtree of collisions in the background
    rtree = backCollisions back
    -- get overlaps of all collision objects and the movement
    collisions = getIntersections movementBB rtree
    playerT = playerTexture cfg
    playerWidth = textureWidth playerT
    playerBB = translate newX newY hb
    -- hitbox of the movement (old position combined with new position)
    movementBB = oldPlayerBB `union` playerBB
    movePlayer' (x, y) b@(BB x1 y1 x2 y2) =
      case dir of
        DUp -> (x, max y (y + (y2 - y1)))
        DDown -> (x, min y (y - (y2 - y1)))
        DLeft -> (max x (x + (x2 - x1)), y)
        DRight -> (min x (x - (x2 - x1)), y)