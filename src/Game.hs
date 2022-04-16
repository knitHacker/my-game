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
    gameState' <- runAppEnv appEnvData updateGameState
    input <- runAppEnv appEnvData stepGame
    let continue = not $ inputStateQuit input
        appEnvData' = appEnvData { appEnvDataInputState = input, appEnvDataGameState = gameState' }


    if continue
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
