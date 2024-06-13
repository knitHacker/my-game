module GameState.Barrier
    ( insertBarriers
    , insertBarrier
    ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import qualified Data.Text as T
import GameState.Collision.BoundBox ( translate, BoundBox )
import GameState.Collision.RTree ( insert, RTree )
import OutputHandles.Types ( TextureEntry, TextureMap )
import Configs ( BarrierCfg(..), PositionCfg(..) )
import Data.Text (Text)


insertBarriers :: T.Text -> PositionCfg -> M.Map T.Text BarrierCfg
              -> TextureMap
              -> M.Map (Int, Int) TextureEntry -> RTree ()
              -> (M.Map (Int, Int) TextureEntry, RTree ())
insertBarriers name loc barrCfgs texts barrs rt = (barrs', rt')
    where
        xPos = x loc
        yPos = y loc
        text = texts ! name
        barrs' = M.insert (xPos, yPos) text barrs
        bCfg = barrCfgs ! name
        rt' = insert (translate xPos yPos (mainHitBox bCfg)) () rt

insertBarrier :: T.Text -> (Int, Int) -> BoundBox -> RTree () -> RTree ()
insertBarrier name loc hitbox = insert (translate xPos yPos hitbox) ()
    where
        xPos = fst loc
        yPos = snd loc