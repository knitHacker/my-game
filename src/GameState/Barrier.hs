module GameState.Barrier
    ( insertBarrier
    ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import qualified Data.Text as T
import GameState.Collision.BoundBox ( translate )
import GameState.Collision.RTree ( insert, RTree )
import OutputHandles.Types ( TextureEntry )
import Configs ( BarrierCfg(..), PositionCfg(..) )


insertBarrier :: T.Text -> PositionCfg -> M.Map T.Text BarrierCfg
              -> M.Map T.Text TextureEntry
              -> M.Map (Int, Int) TextureEntry -> RTree ()
              -> (M.Map (Int, Int) TextureEntry, RTree ())
insertBarrier name aCfg barrCfgs texts barrs rt = (barrs', rt')
    where
        xPos = x aCfg
        yPos = y aCfg
        text = texts ! name
        barrs' = M.insert (xPos, yPos) text barrs
        bCfg = barrCfgs ! name
        rt' = insert (translate xPos yPos (mainHitBox bCfg)) () rt
