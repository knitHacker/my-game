{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env
    ( initAppEnvData
    , runAppEnv
    ) where


import Env.Types
import Configs
import OutputHandles.Types
import InputState
import GameState

import Control.Monad.Reader     (runReaderT)
import Control.Monad.State      (evalStateT, modify)


initAppEnvData :: Configs -> OutputHandles -> IO AppEnvData
initAppEnvData cfgs outs = do
    inputs <- initInputState
    game <- initGameState cfgs outs
    return $ AppEnvData (AppEnvReadData cfgs outs inputs game) (AppEnvMsgData [])


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv (AppEnvData readData msgData) (AppEnv appEnv) = evalStateT (runReaderT appEnv readData) msgData
