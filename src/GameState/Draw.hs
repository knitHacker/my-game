{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( updateWindow
    , drawBoard
    ) where


import Foreign.C.Types
import qualified SDL
import SDL.Vect
import SDL                    (($=))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

import Configs
import GameState
import GameState.Types
import OutputHandles.Types
import OutputHandles.Draw


drawBoard :: (MonadIO m) => Configs -> SDL.Renderer -> [Draw m]
drawBoard cfgs r =
    [ (Black, fmap (\xPos -> drawLine r (xPos, 0) (xPos, screenHeight)) widthPositions)
    , (Black, fmap (\yPos -> drawLine r (0, yPos) (screenWidth, yPos)) heightPositions)
    ]
    where
        screenWidth = fromIntegral $ fst $ configsScreenSize cfgs
        screenHeight = fromIntegral $ snd $ configsScreenSize cfgs
        boardWidth = fst $ configsBoardSize cfgs
        boardHeight = snd $ configsBoardSize cfgs
        intervalW = div screenWidth (fromIntegral boardWidth)
        intervalH = div screenHeight (fromIntegral boardHeight)
        getLinePositions 0 _ _ = []
        getLinePositions n start interval = start : getLinePositions (n-1) (start+interval) interval
        widthPositions = getLinePositions boardWidth 0 intervalW
        heightPositions = getLinePositions boardHeight 0 intervalH


drawPlayer :: (MonadIO m) => Configs -> GameState -> SDL.Renderer -> Draw m
drawPlayer cfgs gs r = (Green, [fillRectangle r rect])
    where
        screenWidth = fromIntegral $ fst $ configsScreenSize cfgs
        screenHeight = fromIntegral $ snd $ configsScreenSize cfgs
        (boardWidth, boardHeight) = configsBoardSize cfgs
        intervalW = div screenWidth (fromIntegral boardWidth)
        intervalH = div screenHeight (fromIntegral boardHeight)
        (xBoard, yBoard) = gameStateUserState gs
        xPos = (fromIntegral xBoard) * intervalW + div intervalW 4
        yPos = (fromIntegral yBoard) * intervalH + div intervalH 4
        width = div intervalW 2
        height = div intervalH 2
        rect = mkRect xPos yPos width height

drawItems :: (MonadIO m) => Configs -> GameState -> SDL.Renderer -> Draw m
drawItems cfgs gs r = (Red, drawItem <$> gameStateItemPositions gs)
    where
        screenWidth = fromIntegral $ fst $ configsScreenSize cfgs
        screenHeight = fromIntegral $ snd $ configsScreenSize cfgs
        (boardWidth, boardHeight) = configsBoardSize cfgs
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
        player = drawPlayer cfgs gs (renderer outputs)
        itemDraw = drawItems cfgs gs (renderer outputs)
        boardDraws = drawBoard cfgs (renderer outputs)
    return (itemDraw : player : boardDraws)
