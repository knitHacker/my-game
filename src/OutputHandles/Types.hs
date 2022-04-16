{-# LANGUAGE OverloadedStrings #-}
module OutputHandles.Types
    ( OutputHandles(..)
    , OutputRead(..)
    , Draw
    , Colour(..)
    ) where


import qualified SDL
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

import Configs
import GameState


data Colour = White | Black | Red | Blue | Green | Yellow

type Draw m = (Colour, [m ()])

data OutputHandles = OutputHandles
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    }


class Monad m => OutputRead m where
    getOutputs :: m OutputHandles
