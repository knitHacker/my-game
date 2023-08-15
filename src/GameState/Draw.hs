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

import Debug.Trace

drawBackground :: Draws -> Configs -> GameArea -> Draws
drawBackground draws cfgs gs = M.insert (0, -1, 0) (Draw t 0 0 boardWidth boardHeight (Just mask)) draws
    where
        back = background gs
        backArea = area back
        t = texture backArea
        xOff = xOffset back
        yOff = yOffset back
        xStart = fromIntegral xOff
        yStart = fromIntegral yOff
        boardWidth = fromIntegral $ boardSizeX cfgs
        boardHeight = fromIntegral $ boardSizeY cfgs
        mask = mkRect xStart yStart boardWidth boardHeight


drawPlayer :: Draws -> GameArea -> Draws
drawPlayer draws gs = M.insert (bottom, 1, xPos) (Draw t xPos yPos pSizeX pSizeY (Just charRect)) draws
    where
        textureEntry = playerTexture $ gameStatePlayer gs
        t = texture textureEntry
        pSizeX = fromIntegral $ textureWidth textureEntry
        pSizeY = fromIntegral $ textureHeight textureEntry
        xOff = xOffset $ background gs
        yOff = yOffset $ background gs
        (xBoard, yBoard) = playerPosition $ gameStatePlayer gs
        xPos = fromIntegral (xBoard - xOff)
        yPos = fromIntegral (yBoard - yOff)
        charRect = getCharacter $ gameStatePlayer gs
        bottom = fromIntegral (yBoard - yOff) + pSizeY

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

drawItems :: Draws -> Configs -> GameArea -> Draws
drawItems draws cfgs gs = foldl (drawItem xOff yOff boardWidth boardHeight) draws (M.elems (gameStateItemManager gs))
    where
        back = background gs
        xOff = xOffset back
        yOff = yOffset back
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs

drawItem :: Int -> Int -> Int -> Int -> Draws -> ItemState -> Draws
drawItem _ _ _ _ d (ItemState _ Nothing) = d
drawItem xStart yStart width height d (ItemState (Item tE _) (Just (xPos, yPos)))
    | yPos + tH < yStart || xPos + tW < xStart || yPos >= yStart + height || xPos >= xStart + width = d
    | otherwise = M.insert (bottom, 2, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
        xPos' = fromIntegral (xPos - xStart)
        yPos' = fromIntegral (yPos - yStart)
        bottom = yPos' + h


drawBarriers :: Draws -> Configs -> GameArea -> Draws
drawBarriers draws cfgs area = foldl (drawBarrier xOff yOff)
                                     draws
                                     (M.elems (backBarriers (background area)))
    where
        back = background area
        xOff = xOffset back
        yOff = yOffset back

drawBarrier :: Int -> Int -> Draws -> ((Int, Int), TextureEntry) -> Draws
drawBarrier xStart yStart d ((xPos, yPos), tE) = M.insert (bottom, 0, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        t = texture tE
        w = fromIntegral $ textureWidth tE
        h = fromIntegral $ textureHeight tE
        xPos' = fromIntegral (xPos - xStart)
        yPos' = fromIntegral (yPos - yStart)
        bottom = yPos'

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m ToRender
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    case gs of
        GameMenu m -> return $ updateGameMenu m
        GameStateArea area -> return $ updateAreaWindow cfgs area


updateAreaWindow :: Configs -> GameArea -> ToRender
updateAreaWindow cfgs area = ToRender draws''' []
    where
        draws = drawBackground mempty cfgs area
        draws' = drawBarriers draws cfgs area
        draws'' = drawPlayer draws' area
        draws''' =  drawItems draws'' cfgs area


updateGameMenu :: Menu -> ToRender
updateGameMenu (Menu words _ _) = ToRender M.empty $ M.elems words
