{-# LANGUAGE OverloadedStrings #-}
module OutputHandles
    ( initOutputHandles
    , cleanupOutputHandles
    , executeDraw
    ) where


import Foreign.C.Types ()
import qualified SDL
import qualified SDL.Image
import SDL.Vect ( V2(V2) )
import SDL                    (($=))
import qualified SDL.Font as Font
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory ()
import Data.Maybe (catMaybes)

import Paths_my_game ()
import Configs
    ( Configs(textureCfgs, gameCfgs),
      ConfigsRead,
      GameConfigs(windowSizeX, windowSizeY, boardSizeX, boardSizeY),
      TextureCfg(file, sizeX, sizeY) )
import OutputHandles.Types
    ( OutputHandles(OutputHandles, font, renderer, window),
      OutputRead,
      TextureEntry(TextureEntry),
      ToRender )
import OutputHandles.Draw ( drawAll, initWindow )
import Env.Files            (getGameFullPath)


fontFile :: FilePath
fontFile = "assets/fonts/InsightSansSSi.ttf"

type TextureMap = M.Map T.Text TextureEntry

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedRenderer
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
    textList <- mapM (loadTexture r) $ M.toList textCfgs
    let textures = M.fromList textList
    print $ fst <$> M.toList textures
    return $ OutputHandles window r textures font ratioX ratioY
    where
        textCfgs = textureCfgs cfgs
        gCfgs = gameCfgs cfgs
        screenWidth = fromIntegral $ windowSizeX gCfgs
        screenHeight = fromIntegral $ windowSizeY gCfgs
        boardX = fromIntegral $ boardSizeX gCfgs
        boardY = fromIntegral $ boardSizeY gCfgs
        ratioX = fromIntegral screenWidth / fromIntegral boardX
        ratioY = fromIntegral screenHeight / fromIntegral boardY

loadTexture :: SDL.Renderer -> (T.Text, TextureCfg) -> IO (T.Text, TextureEntry)
loadTexture r (name, textureCfg) = do
    path <- getGameFullPath $ file textureCfg
    t <- SDL.Image.loadTexture r path
    return (name, TextureEntry (sizeX textureCfg) (sizeY textureCfg) t)

cleanupOutputHandles :: OutputHandles -> IO ()
cleanupOutputHandles outs = do
    Font.free $ font outs
    Font.quit
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit


executeDraw :: (MonadIO m, OutputRead m, ConfigsRead m) => ToRender -> m ()
executeDraw = drawAll
