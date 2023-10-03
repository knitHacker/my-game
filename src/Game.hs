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
import Data.Word

framesPerSecond :: Word32
framesPerSecond = 60

frameTime :: Word32
frameTime = div 1000000000 (framesPerSecond + 1)

runGame :: Word32 -> SystemTime -> AppEnvData -> IO ()
runGame count pTime appEnvData = do
    time <- getSystemTime
    if systemSeconds time /= systemSeconds pTime
        then do
            -- putStrLn $ "Frame rate: " ++ show count
            run 0 time appEnvData
        else do
            let diff = ((systemNanoseconds time) - (systemNanoseconds pTime))
            if diff < frameTime
                then
                    runGame count pTime appEnvData
                else
                    run (count + 1) time appEnvData


run :: Word32 -> SystemTime -> AppEnvData -> IO ()
run count time appEnvData = do
    input <- runAppEnv appEnvData stepGame
    gameState' <- runAppEnv appEnvData updateGameState
    let stop = inputStateQuit input || isGameExiting gameState'
        appEnvData' = appEnvData { appEnvDataInputState = input, appEnvDataGameState = gameState' }

    if not stop
    then do
        runGame count time appEnvData'
    else do
        outputs <- runAppEnv appEnvData getOutputs
        cleanupOutputHandles outputs


stepGame :: AppEnv InputState
stepGame = do
    appEnvData <- ask
    drawsM <- updateWindow
    case drawsM of
        Just draws -> executeDraw draws
        _ -> return ()
    updateInput
