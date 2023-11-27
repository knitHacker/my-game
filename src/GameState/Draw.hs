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

drawBackground :: Draws -> GameConfigs -> GameArea -> Draws
drawBackground draws cfgs gs = M.insert (0, -1, 0) (Draw t 0 0 boardWidth boardHeight (Just mask)) draws
    where
        back = background gs
        t = texture $ backArea back
        xOff = backXOffset back
        yOff = backYOffset back
        xStart = fromIntegral xOff
        yStart = fromIntegral yOff
        boardWidth = fromIntegral $ boardSizeX cfgs
        boardHeight = fromIntegral $ boardSizeY cfgs
        mask = mkRect xStart yStart boardWidth boardHeight


drawPlayer :: Draws -> GameArea -> Draws
drawPlayer draws gs = M.insert (bottom, 2, xPos) (Draw t xPos yPos pSizeX pSizeY (Just charRect)) draws
    where
        player = gameStatePlayer gs
        textureEntry = playerTexture $ playerCfgs player
        t = texture textureEntry
        pSizeX = fromIntegral $ textureWidth textureEntry
        pSizeY = fromIntegral $ textureHeight textureEntry
        xOff = backXOffset $ background gs
        yOff = backYOffset $ background gs
        (xBoard, yBoard) = playerPosition $ playerState player
        xPos = fromIntegral (xBoard - xOff)
        yPos = fromIntegral (yBoard - yOff)
        charRect = getCharacter player
        bottom = fromIntegral (yBoard - yOff) + pSizeY

drawNPC :: Draws -> GameArea -> Draws
drawNPC draws gs = M.insert (bottom, 1, xPos) (Draw t xPos yPos pSizeX pSizeY (Just charRect)) draws
    where
        npcPlayer = npcFollower $ gameStateNPCs gs
        textureEntry = playerTexture $ playerCfgs npcPlayer
        t = texture textureEntry
        pSizeX = fromIntegral $ textureWidth textureEntry
        pSizeY = fromIntegral $ textureHeight textureEntry
        xOff = backXOffset $ background gs
        yOff = backYOffset $ background gs
        (xBoard, yBoard) = playerPosition $ playerState npcPlayer
        xPos = fromIntegral (xBoard - xOff)
        yPos = fromIntegral (yBoard - yOff)
        charRect = getCharacter npcPlayer
        bottom = fromIntegral (yBoard - yOff) + pSizeY



getCharacter :: Player -> SDL.Rectangle CInt
getCharacter player = mkRect xPos yPos width height
    where
        xPos = charSizeX * entryX
        yPos = charSizeY * entryY
        width = charSizeX
        height = charSizeY
        charSizeX = fromIntegral $ textureWidth $ playerTexture $ playerCfgs player
        charSizeY = fromIntegral $ textureHeight $ playerTexture $ playerCfgs player
        (entryY, entryX) = case playerAction $ playerState player of
            PlayerStanding d _ -> (4, getDirectionNum d)
            PlayerMoving (PlayerMove d _ f) -> (getDirectionNum d, fromIntegral f)

-- TODO: change this to derive enum (change order on character sheet)
getDirectionNum :: Direction -> CInt
getDirectionNum DUp = 0
getDirectionNum DDown = 1
getDirectionNum DLeft = 2
getDirectionNum DRight = 3

drawItems :: Draws -> GameConfigs -> GameArea -> Draws
drawItems draws cfgs gs = foldl (drawItem xOff yOff boardWidth boardHeight) draws (M.elems (gameStateItemManager gs))
    where
        back = background gs
        xOff = backXOffset back
        yOff = backYOffset back
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs

drawItem :: Int -> Int -> Int -> Int -> Draws -> ItemState -> Draws
drawItem _ _ _ _ d (ItemState _ Nothing) = d
drawItem xStart yStart width height d (ItemState (Item tE _ _) (Just (xPos, yPos)))
    | yPos + tH < yStart || xPos + tW < xStart || yPos >= yStart + height || xPos >= xStart + width = d
    | otherwise = M.insert (bottom, 5, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
        xPos' = fromIntegral (xPos - xStart)
        yPos' = fromIntegral (yPos - yStart)
        bottom = yPos' + h


drawBarriers :: Draws -> GameConfigs -> GameArea -> Draws
drawBarriers draws cfgs area = foldl (drawBarrier xOff yOff)
                                     draws
                                     (M.elems (backBarriers (background area)))
    where
        back = background area
        xOff = backXOffset back
        yOff = backYOffset back

drawBarrier :: Int -> Int -> Draws -> ((Int, Int), TextureEntry) -> Draws
drawBarrier xStart yStart d ((xPos, yPos), tE) = M.insert (bottom, 0, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        t = texture tE
        w = fromIntegral $ textureWidth tE
        h = fromIntegral $ textureHeight tE
        xPos' = fromIntegral (xPos - xStart)
        yPos' = fromIntegral (yPos - yStart)
        bottom = yPos'

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m (Maybe ToRender)
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    case gs of
        GameMenu m True -> return $ Just $ updateGameMenu m
        GameMenu _ False -> return Nothing
        GameStateArea area True -> return $ Just $ updateAreaWindow cfgs area
        GameStateArea _ False -> return Nothing
        _ -> return $ Just $ ToRender M.empty []


updateAreaWindow :: GameConfigs -> GameArea -> ToRender
updateAreaWindow cfgs area = ToRender draws'''' []
    where
        draws = drawBackground mempty cfgs area
        draws' = drawBarriers draws cfgs area
        draws'' = drawPlayer draws' area
        draws''' = drawNPC draws'' area
        draws'''' =  drawItems draws''' cfgs area


updateGameMenu :: Menu -> ToRender
updateGameMenu (Menu words opts cur) = ToRender M.empty words <> updateMenuOptions cur opts


updateMenuOptions :: MenuCursor -> [MenuAction] -> ToRender
updateMenuOptions (MenuCursor pos tE) ma = ToRender draws $ updateMenuOptions' ma 180
    where
        draws = M.singleton (bottom, 0, xPos') (Draw t xPos' yPos' w h Nothing)
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
        xPos = 80
        yPos = 179 + (20 * pos)
        xPos' = fromIntegral xPos
        yPos' = fromIntegral yPos
        bottom = yPos' + h


updateMenuOptions' :: [MenuAction] -> CInt -> [TextDisplay]
updateMenuOptions' [] _ = []
updateMenuOptions' (h:tl) y = dis : updateMenuOptions' tl newY
    where
        newY = y + 20
        dis = TextDisplay str x y w 15 Blue
        (str, x, w) = case h of
                        GameStart -> ("Start Game", 100, 50)
                        GameExit -> ("Exit", 100, 18)
                        GameContinue _ -> ("Continue", 100, 35)
                        GameStartMenu -> ("Return to Main Menu", 100, 80)
