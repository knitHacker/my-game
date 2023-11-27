module GameState.Player
    ( playerStanding
    , getDirection
    , getBoundBox
    , getPlayerHitbox
    , newPosition
    ) where


import GameState.Collision.BoundBox
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
