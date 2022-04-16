module Utils
    ( randomValue
    ) where


import Control.Monad.IO.Class
import System.Random


randomValue :: (Random a, MonadIO m) => a -> a -> m a
randomValue start end = randomRIO (start, end)
