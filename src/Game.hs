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

-- Time for a frame
frameTime :: Word32
frameTime = div 1000000000 (framesPerSecond + 1)

-- Game loop that enforces a frame rate throttling
runGame :: Word32 -> SystemTime -> AppEnvData -> IO ()
runGame count pTime appEnvData = do
    time <- getSystemTime
    if systemSeconds time /= systemSeconds pTime
        then do
            -- new second, force a new frame so we just have to check
            -- nano second part of time difference
            run 0 time appEnvData
        else do
            let diff = ((systemNanoseconds time) - (systemNanoseconds pTime))
            -- only "run" at the frame rate
            -- this means the "frame rate" limiter limits the speed of the whole game
            -- TODO: maybe in future push this into "output handles" so can still read inputs
            -- in "real" time
            if diff < frameTime
                then
                    -- wait another loop cycle before running game
                    -- TODO: a sleep for frameTime - diff?
                    runGame count pTime appEnvData
                else
                    -- step the state of the game
                    run (count + 1) time appEnvData

-- Run the game for one loop iteration
--      - "step" the game state with the old inputs
--      - 
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
