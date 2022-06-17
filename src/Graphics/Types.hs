module Graphics.Types
    (Graphics(..)
    ) where

import qualified Data.Map.Strict as M
import Data.IORef
import Control.Monad.IO.Class (MonadIO, liftIO)


data Sprite m a = Sprite
    { spritePosition :: a
    , spriteId :: Int
    , draw :: IORef (m ())
    }

data Graphics m a = Graphics
    { nextId :: Int
    , objects :: M.Map (a, Int) (Sprite m a)
    }

initGraphics :: (MonadIO m, MonadIO m2) => m2 (Graphics m a)
initGraphics = return $ Graphics 0 M.empty

addSprite :: (MonadIO m1, MonadIO m2, Ord a) => Graphics m1 a -> a -> m1 () -> m2 (Graphics m1 a, Sprite m1 a)
addSprite (Graphics next map) pos drawF = do
    drawRef <- liftIO $ newIORef drawF
    let sprite = Sprite pos next drawRef
        graphics = M.insert (pos, next) sprite map
    return $ (Graphics (next + 1) graphics, sprite)

removeSprite :: (MonadIO m, Ord a) => Graphics m a -> Sprite m a -> Graphics m a
removeSprite (Graphics next map) (Sprite pos id draw) = Graphics next $ M.delete (pos, id) map

drawGraphics :: (MonadIO m) => Graphics m a -> m ()
drawGraphics (Graphics next map) = mapM_ drawSprite map
    where
        drawSprite (Sprite _ _ draw) = do
            drawF <- liftIO $ readIORef draw
            drawF

class (MonadIO m) => GraphicsDraw m where
    readGraphics :: m (Graphics m2 a)
