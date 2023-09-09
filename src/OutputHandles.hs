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
import qualified SDL.Font as Font
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
import Env.Files            (getGameFullPath)

fontFile :: FilePath
fontFile = "assets/fonts/InsightSansSSi.ttf"

type TextureMap = M.Map T.Text TextureEntry

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


initOutputHandles :: Configs -> IO OutputHandles
initOutputHandles cfgs = do
    fontPath <- getGameFullPath fontFile
    SDL.initialize []
    Font.initialize
    window <- SDL.createWindow "My Game" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window
    r <- SDL.createRenderer window (-1) rendererConfig
    -- clears the screen
    initWindow r
    font <- Font.load fontPath 12
    textures <- loadAreaTextures cfgs r
    itemTextures <- loadItemTextures cfgs r
    barrierTextures <- loadBarrierTextures cfgs r
    textures' <- loadCharTextures (M.union (M.union textures itemTextures) barrierTextures) cfgs r
    -- print $ fst <$> M.toList textures'
    return $ OutputHandles window r textures' font ratioX ratioY
    where
        screenWidth = fromIntegral $ windowSizeX cfgs
        screenHeight = fromIntegral $ windowSizeY cfgs
        boardX = fromIntegral $ boardSizeX cfgs
        boardY = fromIntegral $ boardSizeY cfgs
        ratioX = fromIntegral screenWidth / fromIntegral boardX
        ratioY = fromIntegral screenHeight / fromIntegral boardY

loadTexture :: SDL.Renderer -> (T.Text, TextureCfg) -> IO (T.Text, TextureEntry)
loadTexture r (name, textureCfg) = do
    path <- getGameFullPath $ file textureCfg
    t <- SDL.Image.loadTexture r path
    return (name, TextureEntry (sizeX textureCfg) (sizeY textureCfg) t)

loadCharTextures :: TextureMap -> Configs -> SDL.Renderer -> IO TextureMap
loadCharTextures tm cfgs r = do
    textures <- mapM (loadTexture r . (charTexture <$>)) charCfg
    return $ M.union tm $ M.fromList textures
    where
        charCfg = M.toList $ characters cfgs


loadAreaTextures :: Configs -> SDL.Renderer -> IO TextureMap
loadAreaTextures cfgs r = do
    textures <- mapM (loadTexture r) areasCfg
    return $ M.fromList textures
    where
        areasCfg = M.toList $ areas cfgs

loadItemTextures :: Configs -> SDL.Renderer -> IO TextureMap
loadItemTextures cfgs r = do
    textures <- mapM (loadTexture r)  itemCfg
    return $ M.fromList textures
    where
        itemCfg = M.toList $ items cfgs


loadBarrierTextures :: Configs -> SDL.Renderer -> IO TextureMap
loadBarrierTextures cfgs r = do
    textures <- mapM (loadTexture r) barrierCfg
    return $ M.fromList textures
    where
        barrierCfg = M.toList $ barriers cfgs



cleanupOutputHandles :: OutputHandles -> IO ()
cleanupOutputHandles outs = do
    Font.free $ font outs
    Font.quit
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit


executeDraw :: (MonadIO m, OutputRead m, ConfigsRead m) => ToRender -> m ()
executeDraw draws = drawAll draws
