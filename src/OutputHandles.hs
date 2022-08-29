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

type TextureMap = M.Map T.Text SDL.Texture

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
    textures <- loadAreaTextures cfgs r
    textures' <- loadCharTexture textures cfgs r
    print $ fst <$> M.toList textures'
    return $ OutputHandles window r textures' ratioX ratioY
    where
        screenWidth = fromIntegral $ windowSizeX cfgs
        screenHeight = fromIntegral $ windowSizeY cfgs
        boardX = fromIntegral $ boardSizeX cfgs
        boardY = fromIntegral $ boardSizeY cfgs
        ratioX = fromIntegral screenWidth / fromIntegral boardX
        ratioY = fromIntegral screenHeight / fromIntegral boardY

loadTexture :: SDL.Renderer -> (T.Text, FilePath) -> IO (Maybe (T.Text, SDL.Texture))
loadTexture r (name, textureFile) = do
    path <- getDataFileName textureFile
    fileExists <- doesFileExist path
    if fileExists
    then do
        t <- SDL.Image.loadTexture r path
        return $ Just (name, t)
    else do
        putStrLn $ "Faied to load iamge: " ++ textureFile
        return Nothing

loadCharTexture :: TextureMap -> Configs -> SDL.Renderer -> IO TextureMap
loadCharTexture tm cfgs r =
    case characterFile $ character cfgs of
        Nothing -> return tm
        Just fileName -> do
            textureM <- loadTexture r ("character", fileName)
            case textureM of
                Nothing -> return tm
                Just (name, tex) -> return $ M.insert name tex tm

loadAreaTextures :: Configs -> SDL.Renderer -> IO (M.Map T.Text SDL.Texture)
loadAreaTextures cfgs r = do
    textures <- mapM (loadTexture r) ((fmap areaFile) <$> areasCfg)
    let textures' = catMaybes textures
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
