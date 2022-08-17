{-# LANGUAGE OverloadedStrings #-}
module OutputHandles
    ( initOutputHandles
    , cleanupOutputHandles
    , executeDraw
    ) where


import Foreign.C.Types
import qualified SDL
import qualified SDL.Image
import SDL.Vect
import SDL                    (($=))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import Data.Maybe (catMaybes)

import Paths_my_game
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
    -- clears the screen
    initWindow r
    textures <- loadTextures cfgs r
    return $ OutputHandles window r textures ratioX ratioY
    where
        screenWidth = fromIntegral $ windowSizeX cfgs
        screenHeight = fromIntegral $ windowSizeY cfgs
        boardX = fromIntegral $ boardSizeX cfgs
        boardY = fromIntegral $ boardSizeY cfgs
        ratioX = fromIntegral screenWidth / fromIntegral boardX
        ratioY = fromIntegral screenHeight / fromIntegral boardY

loadTexture :: SDL.Renderer -> (T.Text, AreaCfg) -> IO (Maybe (T.Text, SDL.Texture))
loadTexture r (name, area) = do
    path <- getDataFileName (file area)
    fileExists <- doesFileExist path
    if fileExists
    then do
        t <- SDL.Image.loadTexture r path
        return $ Just (name, t)
    else do
        putStrLn $ "Faied to load iamge: " ++ (file area)
        return Nothing

loadTextures :: Configs -> SDL.Renderer -> IO (M.Map T.Text SDL.Texture)
loadTextures cfgs r = do
    textures <- mapM (loadTexture r) areasCfg
    let textures' = catMaybes textures
    print $ fst <$> textures'
    return $ M.fromList textures'
    where
        areasCfg = M.toList $ areas cfgs

cleanupOutputHandles :: OutputHandles -> IO ()
cleanupOutputHandles outs = do
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit


executeDraw :: (MonadIO m, OutputRead m, ConfigsRead m) => [Draw m] -> m ()
executeDraw draws = do
    outputs <- getOutputs
    drawAll (renderer outputs) draws
