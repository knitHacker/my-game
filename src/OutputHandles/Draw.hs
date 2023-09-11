{-# LANGUAGE OverloadedStrings #-}
module OutputHandles.Draw
    ( mkRect
    , fillRectangle
    , mkPoint
    , setColor
    , drawLine
    , drawAll
    , initWindow
    ) where


import Foreign.C.Types
import qualified SDL
import SDL.Vect
import SDL                    (($=))
import qualified SDL.Font as Font
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M

import Configs
import GameState
import OutputHandles.Types



mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

drawLine :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine r (ox, oy) (tx, ty) =
  SDL.drawLine r (mkPoint ox oy) (mkPoint tx ty)

color :: Color -> Font.Color
color White  = SDL.V4 maxBound maxBound maxBound maxBound
color Black  = SDL.V4 0 0 0 0
color Red    = SDL.V4 maxBound 0 0 maxBound
color Green  = SDL.V4 0 maxBound 0 maxBound
color Blue   = SDL.V4 0 0 maxBound maxBound
color Yellow = SDL.V4 maxBound maxBound 0 maxBound




setColor :: (MonadIO m) => SDL.Renderer -> Color -> m ()
setColor r c = SDL.rendererDrawColor r $= color c


initWindow :: (MonadIO m) => SDL.Renderer -> m ()
initWindow r = do
    setColor r Black
    SDL.clear r
    SDL.present r


drawAll :: (MonadIO m, OutputRead m) => ToRender -> m ()
drawAll drawings = do
    outs <- getOutputs
    let r = renderer outs
        ratX = ratioX outs
        ratY = ratioY outs
        drawings' = scaleDraw ratX ratY <$> draws drawings
        words' = scaleWords ratX ratY <$> drawWords drawings
    SDL.clear r
    mapM_ (draw r) drawings'
    mapM_ (drawText r (font outs)) words'
    SDL.present r

drawText :: MonadIO m => SDL.Renderer -> Font.Font -> TextDisplay -> m ()
drawText r font wd = do
    surf <- Font.solid font (color (wordsColor wd)) (wordsText wd)
    text <- SDL.createTextureFromSurface r surf
    SDL.copy r text Nothing (Just (mkRect (wordsPosX wd) (wordsPosY wd) (wordsWidth wd) (wordsHeight wd)))
    SDL.freeSurface surf

draw :: MonadIO m => SDL.Renderer -> Draw -> m ()
draw r d = do
    SDL.copy r (drawTexture d) (drawMask d) (Just pos)
    where
        pos = mkRect (drawPosX d) (drawPosY d) (drawWidth d) (drawHeight d)


scaleDraw :: Double -> Double -> Draw -> Draw
scaleDraw rX rY (Draw t pX pY w h m) = Draw t (scale pX rX) (scale pY rY) (scale w rX) (scale h rY) m
    where
        scale 0 _ = 0
        scale _ 0 = 0
        scale o r = floor ((fromIntegral o) * r)


scaleWords :: Double -> Double -> TextDisplay -> TextDisplay
scaleWords rX rY (TextDisplay wd pX pY w h c) = TextDisplay wd (scale pX rX) (scale pY rY) (scale w rX) (scale h rY) c
    where
        scale 0 _ = 0
        scale _ 0 = 0
        scale o r = floor ((fromIntegral o) * r)


