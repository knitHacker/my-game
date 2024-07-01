module GameState.Background
    ( updateBackground
    ) where

import Configs
import GameState.Types
import OutputHandles.Types


updateBackground :: GameConfigs -> Background -> Player -> Background
updateBackground cfgs back player = back { backXOffset = getOffset playerX windowX xMax
                                         , backYOffset = getOffset playerY windowY yMax
                                         }
    where
        windowX = boardSizeX cfgs
        windowY = boardSizeY cfgs
        xMax = textureWidth $ backArea back
        yMax = textureHeight $ backArea back
        (playerX, playerY) = playerPosition $ playerState player
        getOffset playerPos window areaMax
            | playerPos < div window 2 = 0
            | playerPos > areaMax - div window 2 = areaMax - window
            | otherwise = playerPos - div window 2
