module GameState.Player
    ( playerStanding
    , getDirection
    , getBoundBox
    , getPlayerHitbox
    ) where


import GameState.Collision.BoundBox
import GameState.Types
import InputState
import Configs


playerStanding :: Player -> Bool
playerStanding player =
    case playerAction (playerState player) of
        PlayerStanding _ -> True
        _ -> False


getDirection :: Player -> Direction
getDirection p = case playerAction (playerState p) of
    PlayerStanding d -> d
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
