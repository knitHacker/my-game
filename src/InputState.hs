module InputState
    ( InputState(..)
    , initInputState
    , InputRead(..)
    , updateInput
    , Direction(..)
    ) where

import qualified SDL
import Control.Monad.IO.Class (MonadIO)


data Direction
    = DUp
    | DDown
    | DLeft
    | DRight
    deriving (Show, Eq)


data InputState = InputState
    { inputStateQuit :: Bool
    , inputStateDirection :: Maybe Direction
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState False Nothing


class Monad m => InputRead m where
    readInputState :: m InputState


updateInput :: (InputRead m, MonadIO m) => m InputState
updateInput = do
    input <- readInputState
    event <- SDL.pollEvent
    case event of
        (Just event) -> return $ newInput event
        _ -> return input


newInput :: SDL.Event -> InputState
newInput (SDL.Event _t p) = payloadToIntent p


payloadToIntent :: SDL.EventPayload -> InputState
payloadToIntent SDL.QuitEvent         = InputState True Nothing
payloadToIntent (SDL.KeyboardEvent k) = InputState False $ getKey k
payloadToIntent _                     = InputState False Nothing


getKey :: SDL.KeyboardEventData -> Maybe Direction
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeUp     -> Just DUp
    SDL.KeycodeDown   -> Just DDown
    SDL.KeycodeLeft   -> Just DLeft
    SDL.KeycodeRight  -> Just DRight
    _                 -> Nothing

