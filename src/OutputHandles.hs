{-# LANGUAGE OverloadedStrings #-}
module OutputHandles
    ( OutputHandles
    , initOutputHandles
    , cleanupOutputHandles
    , OutputRead(..)
    , updateWindow
    ) where


import Foreign.C.Types
import qualified SDL
import SDL.Vect
import SDL                    (($=))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

import Configs
import GameState


data Colour = White | Red | Blue | Green | Yellow

type Draw m = (Colour, [m ()])

data OutputHandles = OutputHandles
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    }

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }

initOutputHandles :: Configs -> IO OutputHandles
initOutputHandles cfg = do
    SDL.initialize []
    window <- SDL.createWindow "My Game" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window
    r <- SDL.createRenderer window (-1) rendererConfig
    let draws = drawBoard cfg r
    drawAll r draws
    return $ OutputHandles window r

drawBoard :: (MonadIO m) => Configs -> SDL.Renderer -> [Draw m]
drawBoard cfg r =
    [ (Blue, fmap (\xPos -> drawLine r (xPos, 0) (xPos, screenHeight)) widthPositions)
    , (Green, fmap (\yPos -> drawLine r (0, yPos) (screenWidth, yPos)) heightPositions)
    ]
    where
        boardWidth = fst $ configsBoardSize cfg
        boardHeight = snd $ configsBoardSize cfg
        intervalW = div screenWidth (fromIntegral boardWidth)
        intervalH = div screenHeight (fromIntegral boardHeight)
        getLinePositions 0 _ _ = []
        getLinePositions n start interval = start : getLinePositions (n-1) (start+interval) interval
        widthPositions = getLinePositions boardWidth 0 intervalW
        heightPositions = getLinePositions boardHeight 0 intervalH


drawPlayer :: (MonadIO m) => Configs -> GameState -> SDL.Renderer -> Draw m
drawPlayer cfgs gs r = (Green, [fillRectangle r rect])
    where
        (boardWidth, boardHeight) = configsBoardSize cfgs
        intervalW = div screenWidth (fromIntegral boardWidth)
        intervalH = div screenHeight (fromIntegral boardHeight)
        (xBoard, yBoard) = gameStateUserState gs
        xPos = (fromIntegral xBoard) * intervalW + div intervalW 4
        yPos = (fromIntegral yBoard) * intervalH + div intervalH 4
        width = div intervalW 2
        height = div intervalH 2
        rect = mkRect xPos yPos width height


mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound


drawLine :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine r (ox, oy) (tx, ty) =
  SDL.drawLine r (mkPoint ox oy) (mkPoint tx ty)


cleanupOutputHandles :: OutputHandles -> IO ()
cleanupOutputHandles outs = do
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit


class Monad m => OutputRead m where
    getOutputs :: m OutputHandles


updateWindow :: (MonadIO m, OutputRead m, ConfigsRead m, GameStateRead m) => m ()
updateWindow = do
    outputs <- getOutputs
    cfgs <- readConfigs
    gs <- readGameState
    let
        player = drawPlayer cfgs gs (renderer outputs)
        boardDraws = drawBoard cfgs (renderer outputs)
    drawAll (renderer outputs) (player : boardDraws)


drawAll :: (MonadIO m) => SDL.Renderer -> [Draw m] -> m ()
drawAll r drawings = do
    setColor r White
    SDL.clear r
    mapM_ (draw r) drawings
    SDL.present r

draw :: (MonadIO m) => SDL.Renderer -> Draw m -> m ()
draw r (color, action) = do
    setColor r color
    mapM_ id action
