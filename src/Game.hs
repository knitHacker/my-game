module Game
    ( runGame
    ) where

import OutputHandles.Types
import OutputHandles
import Env
import Env.Types
import InputState
import GameState
import GameState.Draw

import qualified SDL
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time.Clock.System


runGame :: Int -> SystemTime -> AppEnvData -> IO ()
runGame count pTime appEnvData = do
    time <- getSystemTime
    let count' = if systemSeconds time /= systemSeconds pTime
                  then 0
                  else count + 1
    input <- runAppEnv appEnvData stepGame
    gameState' <- runAppEnv appEnvData updateGameState
    let stop = inputStateQuit input || isGameExiting gameState'
        appEnvData' = appEnvData { appEnvDataInputState = input, appEnvDataGameState = gameState' }

    if not stop
    then do
        runGame count' time appEnvData'
    else do
        outputs <- runAppEnv appEnvData getOutputs
        cleanupOutputHandles outputs


stepGame :: AppEnv InputState
stepGame = do
    appEnvData <- ask
    draws <- updateWindow
    executeDraw draws
    updateInput
