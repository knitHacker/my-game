{-# LANGUAGE OverloadedStrings #-}
module OutputHandles.Draw
    ( mkRect
    , fillRectangle
    , mkPoint
    , setColor
    , drawLine
    , drawAll
    ) where


import Foreign.C.Types
import qualified SDL
import SDL.Vect
import SDL                    (($=))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

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

setColor :: (MonadIO m) => SDL.Renderer -> Colour -> m ()
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Black  = SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound


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
