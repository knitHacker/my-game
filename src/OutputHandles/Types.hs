{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OutputHandles.Types
    ( OutputHandles(..)
    , OutputRead(..)
    , Draw(..)
    , Color(..)
    , TextureEntry(..)
    , Draws
    , ToRender(..)
    , TextDisplay(..)
    ) where

import Foreign.C.Types ( CInt )
import qualified SDL
import qualified SDL.Font as Font
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- first number is layer number
-- second number is y position
-- third number is priority
-- fourth number is x position
type Position = (Int, CInt, Int, CInt)

type Draws = M.Map Position Draw

data ToRender = ToRender
    { draws :: !Draws
    , drawWords :: ![TextDisplay]
    }

instance Monoid ToRender where
    mempty :: ToRender
    mempty = ToRender M.empty []

instance Semigroup ToRender where
   (<>) :: ToRender -> ToRender -> ToRender
   (<>) (ToRender m1 l1) (ToRender m2 l2) = ToRender (m1 <> m2) (l1 <> l2)

data Color = White | Black | Red | Blue | Green | Yellow

data Draw = Draw
    { drawTexture :: SDL.Texture
    , drawPosX :: CInt
    , drawPosY :: CInt
    , drawWidth :: CInt
    , drawHeight :: CInt
    , drawMask :: Maybe (SDL.Rectangle CInt)
    }


data TextDisplay = TextDisplay
    { wordsText :: T.Text
    , wordsPosX :: CInt
    , wordsPosY :: CInt
    , wordsWidth :: CInt
    , wordsHeight :: CInt
    , wordsColor :: Color
    }


data TextureEntry = TextureEntry
    { textureWidth :: Int
    , textureHeight :: Int
    , texture :: SDL.Texture
    }


data OutputHandles = OutputHandles
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , textures :: M.Map T.Text TextureEntry
    , font :: Font.Font
    , ratioX :: Double
    , ratioY :: Double
    }


class Monad m => OutputRead m where
    getOutputs :: m OutputHandles
