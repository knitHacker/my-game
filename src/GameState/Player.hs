module GameState.Player
    ( playerStanding
    , getDirection
    , getBoundBox
    ) where


import GameState.Collision.BoundBox
import GameState.Types
import InputState
import Configs


playerStanding :: Player -> Bool
playerStanding (Player _ _ _ (Left _) _ _) = True
playerStanding _ = False


getDirection :: Player -> Direction
getDirection p = case playerMovement p of
    Left d -> d
    Right (PlayerMove d _ _) -> d


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
