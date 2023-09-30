module GameState.Player
    ( playerStanding
    , getDirection
    ) where


import GameState.Types
import InputState


playerStanding :: Player -> Bool
playerStanding (Player _ _ _ (Left _) _ _) = True
playerStanding _ = False


getDirection :: Player -> Direction
getDirection p = case playerMovement p of
    Left d -> d
    Right (PlayerMove d _ _) -> d


