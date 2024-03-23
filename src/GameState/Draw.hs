{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( updateWindow
    ) where


import Foreign.C.Types ( CInt )
import qualified SDL
import SDL.Vect ()
import SDL                    (($=))
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Text as T
import Data.Unique ( Unique )

import Configs
    ( ConfigsRead(..)
    , GameConfigs(..)
    )
import GameState ()
import GameState.Types
    ( Background(..)
    , ItemState(..)
    , Item(..)
    , ItemManager(..)
    , Player(..)
    , PlayerState(..)
    , PlayerConfig(..)
    , GameStateRead(..)
    , NPCManager(..)
    , GameArea(..)
    , MenuCursor(..)
    , Menu(..)
    , PlayerMovement(..)
    , PlayerAction(..)
    , MenuAction(..)
    , Inventory(..)
    , GameState(..)
    )
import OutputHandles.Types
    ( Color(..)
    , Draw(..)
    , Draws
    , TextDisplay(..)
    , TextureEntry(..)
    , ToRender(..)
    )
import OutputHandles.Draw ( mkRect )
import InputState ( Direction(..) )

import Debug.Trace ()

drawBackground :: Draws -> GameConfigs -> GameArea -> Draws
drawBackground draws cfgs gs = M.insert (0, 0, -1, 0) (Draw t 0 0 boardWidth boardHeight (Just mask)) draws
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
drawPlayer draws gs = M.insert (0, bottom, 2, xPos) (Draw t xPos yPos pSizeX pSizeY (Just charRect)) draws
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
drawNPC draws gs = M.insert (0, bottom, 1, xPos) (Draw t xPos yPos pSizeX pSizeY (Just charRect)) draws
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
drawItems draws cfgs gs = foldl (drawItem (itemHighlighted im) xOff yOff boardWidth boardHeight) draws (M.assocs (itemMap im))
    where
        im = gameStateItemManager gs
        back = background gs
        xOff = backXOffset back
        yOff = backYOffset back
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs

drawItem :: Maybe Unique -> Int -> Int -> Int -> Int -> Draws -> (Unique, ItemState) -> Draws
drawItem _ _ _ _ _ d (_, ItemState _ Nothing) = d
drawItem hKey xStart yStart width height d (key, ItemState (Item _ tE tEh _ _ _) (Just (xPos, yPos)))
    | yPos + tH < yStart || xPos + tW < xStart || yPos >= yStart + height || xPos >= xStart + width = d
    | otherwise = M.insert (0, bottom, 5, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        tE' = case hKey of
                Just hk -> if hk == key then tEh else tE
                Nothing -> tE
        t = texture tE'
        tW = textureWidth tE'
        tH = textureHeight tE'
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
drawBarrier xStart yStart d ((xPos, yPos), tE) = M.insert (0, bottom, 0, xPos') (Draw t xPos' yPos' w h Nothing) d
    where
        t = texture tE
        w = fromIntegral $ textureWidth tE
        h = fromIntegral $ textureHeight tE
        xPos' = fromIntegral (xPos - xStart)
        yPos' = fromIntegral (yPos - yStart)
        bottom = yPos' + h

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m (Maybe ToRender)
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    case gs of
        GameMenu m True -> return $ Just $ updateGameMenu m
        GameMenu _ False -> return Nothing
        GameStateArea area True -> return $ Just $ updateAreaWindow cfgs area
        GameStateArea _ False -> return Nothing
        GameInventory inv -> return $ Just $ updateInventory cfgs inv
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
        draws = M.singleton (0, bottom, 0, xPos') (Draw t xPos' yPos' w h Nothing)
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

updateInventory :: GameConfigs -> Inventory -> ToRender
updateInventory cfgs inv = current <> ToRender draws' texts
    where
        bagE = bagTexture inv
        boardWidth = boardSizeX cfgs
        boardHeight = boardSizeY cfgs
        draws = M.singleton (1, 100, 0, 0) (Draw (texture bagE) 0 0 (fromIntegral boardWidth) (fromIntegral boardHeight) Nothing)
        (draws', texts) = if M.size itemMap > 0
                    then (itemDraw, numText)
                    else (draws, [])
        itemDraw = M.insert (2,1,1,1) (Draw (texture $ itemTexture $ item) (fromIntegral itemX) (fromIntegral itemY) 50 50 Nothing) draws
        numText = [TextDisplay countStr (fromIntegral (itemX + 25)) (fromIntegral (itemY + 60)) (fromIntegral (10 * (T.length countStr))) 20 Red
                  , TextDisplay itemStr (fromIntegral (itemX - 15)) (fromIntegral (itemY - 25)) (fromIntegral (10 * (T.length itemStr))) 20 Red]
        countStr = T.pack $ show count
        itemStr = itemName item
        area = areaInfo inv
        current = updateAreaWindow cfgs area
        itemMap = playerItems $ playerState $ gameStatePlayer area
        (item, count) = head $ M.assocs itemMap
        itemX = boardWidth `div` 2 - 25
        itemY = boardHeight `div` 2 - 25
