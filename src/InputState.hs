module InputState
    ( InputState(..)
    , initInputState
    , InputRead(..)
    , updateInput
    , Direction(..)
    , escapePressed
    , moveInputPressed
    ) where

import qualified SDL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Data.Time.Clock.System

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
    , inputTimestamp :: Word32
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState False Nothing False False False 0


class Monad m => InputRead m where
    readInputState :: m InputState

escapePressed :: InputState -> Bool
escapePressed (InputState _ _ _ True _ _) = True
escapePressed _ = False

moveInputPressed :: InputState -> Bool
moveInputPressed (InputState _ Nothing _ _ _ _) = True
moveInputPressed _ = False


updateInput :: (InputRead m, MonadIO m) => m InputState
updateInput = do
    input <- readInputState
    time <- liftIO getSystemTime
    event <- SDL.pollEvent
    let ts = div (systemNanoseconds time) 1000000
    case event of
        (Just event) -> return $ payloadToIntent event ts
        _ -> return $ input { inputRepeat = True, inputTimestamp = ts }


payloadToIntent :: SDL.Event -> Word32 -> InputState
payloadToIntent (SDL.Event _ SDL.QuitEvent) ts = InputState True Nothing False False False ts
payloadToIntent (SDL.Event _ (SDL.KeyboardEvent k)) ts =
    case getKey k of
        Nothing -> InputState False Nothing False False False ts
        Just (r, Left EnterPress) -> InputState False Nothing True False r ts
        Just (r, Left EscapePress) -> InputState False Nothing False True r ts
        Just (r, Right d) -> InputState False (Just d) False False r ts
payloadToIntent (SDL.Event _ _) ts = InputState False Nothing False False False ts


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

