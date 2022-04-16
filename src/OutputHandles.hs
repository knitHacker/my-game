{-# LANGUAGE OverloadedStrings #-}
module OutputHandles
    ( initOutputHandles
    , cleanupOutputHandles
    , executeDraw
    ) where


import Foreign.C.Types
import qualified SDL
import SDL.Vect
import SDL                    (($=))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

import Configs
import GameState
import GameState.Draw
import OutputHandles.Types
import OutputHandles.Draw


rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }

initOutputHandles :: Configs -> IO OutputHandles
initOutputHandles cfgs = do
    SDL.initialize []
    window <- SDL.createWindow "My Game" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window
    r <- SDL.createRenderer window (-1) rendererConfig
    let draws = drawBoard cfgs r
    drawAll r draws
    return $ OutputHandles window r
    where
        screenWidth = fromIntegral $ fst $ configsScreenSize cfgs
        screenHeight = fromIntegral $ snd $ configsScreenSize cfgs


cleanupOutputHandles :: OutputHandles -> IO ()
cleanupOutputHandles outs = do
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit


executeDraw :: (MonadIO m, OutputRead m) => [Draw m] -> m ()
executeDraw draws = do
    outputs <- getOutputs
    drawAll (renderer outputs) draws
