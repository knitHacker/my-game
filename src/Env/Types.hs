{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env.Types
    ( AppEnvReadData(..)
    , AppEnvMsgData(..)
    , AppEnvData(..)
    , AppEnv(..)
    ) where

import Configs
import OutputHandles.Types
import InputState
import GameState.Types

import Control.Monad.Reader          (MonadReader, ReaderT, asks)
import Control.Monad
import Control.Monad.State           (MonadState, StateT, MonadIO, gets, modify)


data AppEnvReadData = AppEnvReadData
    { appEnvConfigs :: Configs
    , appEnvOutputHandles :: OutputHandles
    , appEnvInputState :: InputState
    , appEnvGameState :: GameState
    }

data AppEnvMsgData = AppEnvMsgData
    { appEnvMsgs :: [String]
    }

data AppEnvData = AppEnvData
    { appEnvReadData :: AppEnvReadData
    , appEnvMsgData :: AppEnvMsgData
    }

newtype AppEnv a = AppEnv (ReaderT AppEnvReadData (StateT AppEnvMsgData IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppEnvReadData
        , MonadState AppEnvMsgData
        , MonadIO
        )

instance OutputRead AppEnv where
    getOutputs :: AppEnv OutputHandles
    getOutputs = asks appEnvOutputHandles

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = asks appEnvInputState

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv Configs
    readConfigs = asks appEnvConfigs

instance GameStateRead AppEnv where
    readGameState :: AppEnv GameState
    readGameState = asks appEnvGameState
