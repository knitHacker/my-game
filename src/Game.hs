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


runGame :: AppEnvData -> IO ()
runGame appEnvData = do
    input <- runAppEnv appEnvData stepGame
    gameState' <- runAppEnv appEnvData updateGameState
    let stop = inputStateQuit input || isGameExiting gameState'
        appEnvData' = appEnvData { appEnvDataInputState = input, appEnvDataGameState = gameState' }


    if not stop
    then do
        runGame appEnvData'
    else do
        outputs <- runAppEnv appEnvData getOutputs
        cleanupOutputHandles outputs


stepGame :: AppEnv InputState
stepGame = do
    appEnvData <- ask
    draws <- updateWindow
    executeDraw draws
    updateInput
