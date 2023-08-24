module InputState
    ( InputState(..)
    , initInputState
    , InputRead(..)
    , updateInput
    , Direction(..)
    ) where

import qualified SDL
import Control.Monad.IO.Class (MonadIO)

import Debug.Trace

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
    , inputRepeat :: Bool
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState False Nothing False False False


class Monad m => InputRead m where
    readInputState :: m InputState


updateInput :: (InputRead m, MonadIO m) => m InputState
updateInput = do
    input <- readInputState
    event <- SDL.pollEvent
    case event of
        (Just event) -> return $ newInput event
        _ -> return $ input { inputRepeat = True }


newInput :: SDL.Event -> InputState
newInput (SDL.Event _ p) = payloadToIntent p


payloadToIntent :: SDL.EventPayload -> InputState
payloadToIntent SDL.QuitEvent         = InputState True Nothing False False False
payloadToIntent (SDL.KeyboardEvent k) =
    case getKey k of
        Nothing -> InputState False Nothing False False False
        Just (r, Left EnterPress) -> InputState False Nothing True False r
        Just (r, Left EscapePress) -> InputState False Nothing False True r
        Just (r, Right d) -> InputState False (Just d) False False r
payloadToIntent _                     = InputState False Nothing False False False


getKey :: SDL.KeyboardEventData -> Maybe (Bool, Either KeyPress Direction)
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed repeat keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeUp     -> Just (repeat, Right DUp)
    SDL.KeycodeDown   -> Just (repeat, Right DDown)
    SDL.KeycodeLeft   -> Just (repeat, Right DLeft)
    SDL.KeycodeRight  -> Just (repeat, Right DRight)
    SDL.KeycodeReturn -> Just (repeat, Left EnterPress)
    SDL.KeycodeEscape -> Just (repeat, Left EscapePress)
    _                 -> Nothing

