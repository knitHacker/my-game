{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( updateWindow
    ) where


import Foreign.C.Types
import qualified SDL
import SDL.Vect
import SDL                    (($=))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Configs
import GameState
import GameState.Types
import OutputHandles.Types
import OutputHandles.Draw
import InputState

xBlocksShow = 10
yBlocksShow = 10


drawBackground :: (MonadIO m) => Configs -> GameState -> OutputHandles -> [Draw m]
drawBackground cfgs gs outs = [Texture (texture (area (background gs))) (Just mask) Nothing]
    where
        mask = mkRect xStart yStart xEnd yEnd
        xOff = xOffset $ background gs
        yOff = yOffset $ background gs
        xStart = fromIntegral xOff
        yStart = fromIntegral yOff
        xEnd = fromIntegral boardWidth
        yEnd = fromIntegral boardHeight

        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs


drawPlayer :: (MonadIO m) => GameState -> OutputHandles -> Draw m
drawPlayer gs outs = Texture t (Just charRect) (Just rect)
    where
        textureEntry = playerTexture $ gameStatePlayer gs
        t = texture textureEntry
        pSizeX = fromIntegral $ textureWidth textureEntry
        pSizeY = fromIntegral $ textureHeight textureEntry
        xOff = xOffset $ background gs
        yOff = yOffset $ background gs
        (xBoard, yBoard) = playerPosition $ gameStatePlayer gs
        xRat = ratioX outs
        yRat = ratioY outs
        xPos = floor (((fromIntegral (xBoard - xOff)) - pSizeX / 2) * xRat)
        yPos = floor (((fromIntegral (yBoard - yOff)) - pSizeY / 2) * yRat)
        width = floor (xRat * pSizeX)
        height = floor (yRat * pSizeY)
        rect = mkRect xPos yPos width height
        charRect = getCharacter $ gameStatePlayer gs

getCharacter :: Player -> SDL.Rectangle CInt
getCharacter player = mkRect xPos yPos width height
    where
        xPos = charSizeX * entryX
        yPos = charSizeY * entryY
        width = charSizeX
        height = charSizeY
        charSizeX = fromIntegral $ textureWidth $ playerTexture player
        charSizeY = fromIntegral $ textureHeight $ playerTexture player
        (entryY, entryX) = case playerMovement player of
            Left d -> (4, getDirectionNum d)
            Right (d, _, f) -> (getDirectionNum d, fromIntegral f)

-- TODO: change this to derive enum (change order on character sheet)
getDirectionNum :: Direction -> CInt
getDirectionNum DUp = 0
getDirectionNum DDown = 1
getDirectionNum DLeft = 2
getDirectionNum DRight = 3

drawItems :: (MonadIO m) => Configs -> GameState -> SDL.Renderer -> Draw m
drawItems cfgs gs r = Graphic Red (drawItem <$> M.keys (gameItems (gameStateItemManager gs)))
    where
        screenWidth = fromIntegral $ windowSizeX cfgs
        screenHeight = fromIntegral $ windowSizeY cfgs
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs
        intervalW = div screenWidth (fromIntegral boardWidth)
        intervalH = div screenHeight (fromIntegral boardHeight)
        getXPos xPos = (fromIntegral xPos) * intervalW + div intervalW 4
        getYPos yPos = (fromIntegral yPos) * intervalH + div intervalH 4
        width = div intervalW 2
        height = div intervalH 2
        drawItem (xPos, yPos) = fillRectangle r $ mkRect (getXPos xPos) (getYPos yPos) width height


updateWindow :: (MonadIO m, OutputRead m, ConfigsRead m, GameStateRead m) => m [Draw m]
updateWindow = do
    outputs <- getOutputs
    cfgs <- readConfigs
    gs <- readGameState
    let
        boardDraws = drawBackground cfgs gs outputs
        player = drawPlayer gs outputs
        -- itemDraw = drawItems cfgs gs (renderer outputs)
    return (boardDraws ++ [] ++ [player])
