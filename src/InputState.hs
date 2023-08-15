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

data KeyPress
    = EnterPress
    | EscapePress


data InputState = InputState
    { inputStateQuit :: Bool
    , inputStateDirection :: Maybe Direction
    , inputStateEnter :: Bool
    , inputStateEsc :: Bool
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState False Nothing False False


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
payloadToIntent SDL.QuitEvent         = InputState True Nothing False False
payloadToIntent (SDL.KeyboardEvent k) =
    case getKey k of
        Nothing -> InputState False Nothing False False
        Just (Left EnterPress) -> InputState False Nothing True False
        Just (Left EscapePress) -> InputState False Nothing False True
        Just (Right d) -> InputState False (Just d) False False
payloadToIntent _                     = InputState False Nothing False False


getKey :: SDL.KeyboardEventData -> Maybe (Either KeyPress Direction)
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeUp     -> Just $ Right DUp
    SDL.KeycodeDown   -> Just $ Right DDown
    SDL.KeycodeLeft   -> Just $ Right DLeft
    SDL.KeycodeRight  -> Just $ Right DRight
    SDL.KeycodeReturn -> Just $ Left EnterPress
    SDL.KeycodeEscape -> Just $ Left EscapePress
    _                 -> Nothing

