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


drawBackground :: Draws -> Configs -> GameState -> OutputHandles -> Draws
drawBackground draws cfgs gs outs = M.insert (-1, -1, -1) (Draw t 0 0 w h (Just mask)) draws
    where
        back = background gs
        backArea = area back
        t = texture backArea
        w = floor $ xRat * (fromIntegral (textureWidth backArea))
        h = floor $ yRat * (fromIntegral (textureHeight backArea))
        xOff = xOffset back
        yOff = yOffset back
        xStart = fromIntegral xOff
        yStart = fromIntegral yOff
        boardWidth = fromIntegral $ boardSizeX cfgs
        boardHeight = fromIntegral $ boardSizeY cfgs
        mask = mkRect xStart yStart boardWidth boardHeight
        xRat = ratioX outs
        yRat = ratioY outs


drawPlayer :: Draws -> GameState -> OutputHandles -> Draws
drawPlayer draws gs outs = M.insert (yPos, xPos, 0) (Draw t xPos yPos width height (Just charRect)) draws
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

drawItems :: Draws -> Configs -> GameState -> SDL.Renderer -> Draws
drawItems draws cfgs gs r = undefined
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


updateWindow :: (MonadIO m, OutputRead m, ConfigsRead m, GameStateRead m) => m Draws
updateWindow = do
    outputs <- getOutputs
    cfgs <- readConfigs
    gs <- readGameState
    let
        draws = drawBackground mempty cfgs gs outputs
        draws' = drawPlayer draws gs outputs
        -- itemDraw = drawItems cfgs gs (renderer outputs)
    return draws'
