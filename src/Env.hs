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


initAppEnvData :: Configs -> OutputHandles -> IO AppEnvData
initAppEnvData cfgs outs = do
    inputs <- initInputState
    game <- initGameState cfgs outs
    return $ AppEnvData cfgs outs inputs game


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = runReaderT appEnv appEnvData
