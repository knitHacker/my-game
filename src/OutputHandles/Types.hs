{-# LANGUAGE OverloadedStrings #-}
module OutputHandles.Types
    ( OutputHandles(..)
    , OutputRead(..)
    , Draw(..)
    , Colour(..)
    , TextureEntry(..)
    ) where

import Foreign.C.Types
import qualified SDL
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T


data Colour = White | Black | Red | Blue | Green | Yellow

data Draw m = Graphic Colour [m ()] | Texture SDL.Texture (Maybe (SDL.Rectangle CInt)) (Maybe (SDL.Rectangle CInt))

data TextureEntry = TextureEntry
    { textureWidth :: Int
    , textureHeight :: Int
    , texture :: SDL.Texture
    }


data OutputHandles = OutputHandles
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , textures :: M.Map T.Text TextureEntry
    , ratioX :: Double
    , ratioY :: Double
    }


class Monad m => OutputRead m where
    getOutputs :: m OutputHandles
