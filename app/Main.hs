{-# LANGUAGE OverloadedStrings #-}
module Main where

import Env
import Configs
import OutputHandles
import Game

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified SDL


main :: IO ()
main = do
    configs <- initConfigs
    outs <- initOutputHandles configs
    appEnvData <- initAppEnvData configs outs
    runGame appEnvData
