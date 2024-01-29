module GameState.Player
  ( playerStanding,
    getDirection,
    getBoundBox,
    getPlayerHitbox,
    newPosition,
    movePlayer,
  )
where

import Configs
    ( CharacterMovement(moveStep),
      CharacterHitBoxes(sideHitbox, frontHitbox) )
import Data.Word (Word32)
import GameState.Collision.BoundBox
    ( BoundBox(BB), union, translate )
import GameState.Collision.RTree ( getIntersections )
import GameState.Types
    ( Background(backCollisions),
      Player(Player, playerState),
      PlayerState(playerAction, playerPosition),
      PlayerConfig(playerMoveCfgs, playerHitBoxes, playerTexture),
      PlayerMovement(PlayerMove),
      PlayerAction(PlayerMoving, PlayerStanding) )
import InputState ( Direction(..) )
import OutputHandles.Types ( TextureEntry(textureWidth) )

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
movePlayer back player@(Player cfg state) dir ts f (newX, newY) = player {playerState = state {playerAction = PlayerMoving (PlayerMove dir ts f), playerPosition = newPos}}
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
    movementBB = union oldPlayerBB playerBB
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
        DUp -> (x, min y (y + (y2 - y1)))
        DDown -> (x, max y (y - (y2 - y1)))
        DLeft -> (min x (x + (x2 - x1)), y)
        DRight -> (max x (x - (x2 - x1)), y)