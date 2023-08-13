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
    , inputStateEnter :: Bool
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState False Nothing False


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
payloadToIntent SDL.QuitEvent         = InputState True Nothing False
payloadToIntent (SDL.KeyboardEvent k) =
    case getKey k of
        Left isEnter -> InputState False Nothing isEnter
        Right d -> InputState False (Just d) False
payloadToIntent _                     = InputState False Nothing False


getKey :: SDL.KeyboardEventData -> Either Bool Direction
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Left False
getKey (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeUp     -> Right DUp
    SDL.KeycodeDown   -> Right DDown
    SDL.KeycodeLeft   -> Right DLeft
    SDL.KeycodeRight  -> Right DRight
    SDL.KeycodeReturn -> Left True
    _                 -> Left False

