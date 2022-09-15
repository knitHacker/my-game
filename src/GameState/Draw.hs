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


drawBackground :: Draws -> Configs -> GameState -> Draws
drawBackground draws cfgs gs = M.insert (0, -1, 0) (Draw t 0 0 w h (Just mask)) draws
    where
        back = background gs
        backArea = area back
        t = texture backArea
        w = fromIntegral $ textureWidth backArea
        h = fromIntegral $ textureHeight backArea
        xOff = xOffset back
        yOff = yOffset back
        xStart = fromIntegral xOff
        yStart = fromIntegral yOff
        boardWidth = fromIntegral $ boardSizeX cfgs
        boardHeight = fromIntegral $ boardSizeY cfgs
        mask = mkRect xStart yStart boardWidth boardHeight


drawPlayer :: Draws -> GameState -> Draws
drawPlayer draws gs = M.insert (bottom, 0, xPos) (Draw t xPos yPos pSizeX pSizeY (Just charRect)) draws
    where
        textureEntry = playerTexture $ gameStatePlayer gs
        t = texture textureEntry
        pSizeX = fromIntegral $ textureWidth textureEntry
        pSizeY = fromIntegral $ textureHeight textureEntry
        xOff = xOffset $ background gs
        yOff = yOffset $ background gs
        (xBoard, yBoard) = playerPosition $ gameStatePlayer gs
        xPos = (fromIntegral (xBoard - xOff)) - div pSizeX 2
        yPos = (fromIntegral (yBoard - yOff)) - div pSizeY 2
        charRect = getCharacter $ gameStatePlayer gs
        bottom = (fromIntegral (yBoard - yOff)) + (div pSizeY 2)

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

drawItems :: Draws -> Configs -> GameState -> Draws
drawItems draws cfgs gs = foldl (drawItem xOff yOff boardWidth boardHeight) draws (M.toList (gameItems (gameStateItemManager gs)))
    where
        back = background gs
        backArea = area back
        t = texture backArea
        w = fromIntegral $ textureWidth backArea
        h = fromIntegral $ textureHeight backArea
        xOff = xOffset back
        yOff = yOffset back
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs

drawItem :: Int -> Int -> Int -> Int -> Draws -> ((Int, Int), Item) -> Draws
drawItem xStart yStart width height d ((xPos, yPos), (Item tE _))
    | yPos < yStart || xPos < xStart || yPos > yStart + height || xPos > xStart + width = d
    | otherwise = M.insert (bottom, 1, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        t = texture tE
        w = fromIntegral $ textureWidth tE
        h = fromIntegral $ textureWidth tE
        xPos' = (fromIntegral (xPos - xStart)) - div w 2
        yPos' = (fromIntegral (yPos - yStart)) - div h 2
        bottom = yPos' + h


updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m Draws
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    let
        draws = drawBackground mempty cfgs gs
        draws' = drawPlayer draws gs
        draws'' =  drawItems draws' cfgs gs
    return draws''
