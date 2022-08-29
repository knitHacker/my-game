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

xBlocksShow = 10
yBlocksShow = 10


drawBackground :: (MonadIO m) => Configs -> GameState -> OutputHandles -> [Draw m]
drawBackground cfgs gs outs = [Texture ((textures outs) ! "outside") (Just mask) Nothing]
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


drawPlayer :: (MonadIO m) => OutputHandles -> GameState -> Draw m
drawPlayer outs gs =
    case M.lookup "character" (textures outs) of
        Nothing -> allBack
        Just t -> Texture t Nothing $ Just rect
    where
        r = renderer outs
        fallBack = Graphic Green [fillRectangle r rect]
        sizeX = 10
        sizeY = 10
        xOff = xOffset $ background gs
        yOff = yOffset $ background gs
        (xBoard, yBoard) = playerPosition $ gameStatePlayer gs
        xRat = ratioX outs
        yRat = ratioY outs
        xPos = floor ((fromIntegral (xBoard - xOff)) * xRat - sizeX / 2)
        yPos = floor ((fromIntegral (yBoard - yOff)) * yRat - sizeY / 2)
        width = floor (xRat * sizeX)
        height = floor (yRat * sizeY)
        rect = mkRect xPos yPos width height

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
        player = drawPlayer outputs gs
        -- itemDraw = drawItems cfgs gs (renderer outputs)
    return (boardDraws ++ [] ++ [player])
