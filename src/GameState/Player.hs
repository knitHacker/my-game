module GameState.Player
    ( playerStanding
    , getDirection
    , getBoundBox
    , getPlayerHitbox
    , newPosition
    , movePlayer
    ) where

import Data.Word (Word32)
import OutputHandles.Types
import GameState.Collision.BoundBox
import GameState.Collision.RTree
import GameState.Types
import InputState
import Configs

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
movePlayer back player@(Player cfg state) dir ts f (newX, newY) = player { playerState = state { playerAction = PlayerMoving (PlayerMove dir ts f), playerPosition = newPos}}
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


