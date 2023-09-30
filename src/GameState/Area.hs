module GameState.Area
    ( updateArea
    ) where

import Data.Word (Word32)
import qualified Data.Map.Strict as M

import Configs
import InputState
import OutputHandles.Types
import GameState.Collision.BoundBox
import GameState.Collision.RTree
import GameState.Player
import GameState.Types

updateArea :: OutputHandles -> Configs -> InputState -> GameArea -> GameState
updateArea outs cfgs inputs area
    | escapePressed inputs = GameMenu (initPauseMenu outs area) True
    | moveInputPressed inputs && playerStanding (gameStatePlayer area) = GameState a False
    | otherwise = GameStateArea (area'' { background = background' }) True
    where
        player = gameStatePlayer area
        (moved, player') = case (inputStateDirection inputs, playerMovement player) of
            (Nothing, Left _) -> (False, player)
            (Nothing, Right (d, _, _)) -> (False, player { playerMovement = Left d })
            (Just dir, _) -> (True, updatePlayer (background area) (gameStatePlayer area) dir)
        area' = (area { gameStatePlayer = player' })
        area'' = if moved then collisionCheck area player' else area'
        background' = updateBackground cfgs (background area'') player'



updatePlayer :: Background -> InputState -> Player -> (Bool, Player)
updatePlayer back inputs p@(Player hb pos mv cfgs) =
    case (inputStateDirection inputs, mv) of
        (Nothing, Left) -> (False, p)
        (Nothing, Right (PlayerMove d _ _)) -> (False, player { playerMovement = Left d})
        (Just iDir, Right pm@(PlayerMove _ _ _))
            | newDir == oldDir && timeDiff > rate -> undefined
            | newDir == oldDir ->  (False, player)
            | otherwise -> player { playerPosition = newPosition back player dir, playerMovement = Right (dir, 0, 0) }
    where
        timeDiff = ts - oldTs
        newPos = newPosition newDir player
        rate = stepRate $ playerCfgs player


movePlayer :: Background -> Player -> Direction -> (Int, Int) -> Player
movePlayer back player dir (newX, newY) = undefined
    where
        rtree = backCollisions back
        collisions = getCollision playerBB rtree
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        x1 = newX
        y2 = newY
        y1 = y2 - collideHeight $ playerHitBox player
        x2 = x1 + playerWidth -- collideWidth $ playerHitBox player
        playerBB = bb x1 y1 x2 y2

newPosition :: Background -> Player -> Direction -> (Int, Int)
newPosition back player dir = (x'', y'')
    where
        charSizeX = textureWidth (playerTexture player)
        charSizeY = textureHeight (playerTexture player)
        moveAmt = moveStep $ playerCfgs player
        (xMove, yMove) = updatePosition moveAmt dir
        xMax = (textureWidth $ area back) - charSizeX
        yMax = (textureHeight $ area back) - charSizeY
        xMin = 0
        yMin = 0
        (x, y) = playerPosition player
        x' = x + xMove
        y' = y + yMove
        x'' = max (min x' xMax) xMin
        y'' = max (min y' yMax) yMin


updatePosition :: Int -> Direction -> (Int, Int)
updatePosition m DUp = (0, -m)
updatePosition m DDown = (0, m)
updatePosition m DLeft = (-m, 0)
updatePosition m DRight = (m, 0)

{-
updatePlayer :: Background -> Player -> Direction -> Player
updatePlayer back player@(Player cfg bb pos (Right (d, l, f)) items mCfg) newDir
    | d == newDir && l >= rate = player {playerPosition =
        newPosition back player newDir, playerMovement = Right (newDir, 0, mod (f + 1) 8 ) }
    | d == newDir = player {playerMovement = Right (d, l + 1, f)}
    | otherwise = player { playerPosition = newPosition back player newDir, playerMovement = Right (newDir, 0, 0) }
    where
        rate = stepRate mCfg
updatePlayer back player dir = player { playerPosition = newPosition back player dir, playerMovement = Right (dir, 0, 0) }
-}

updateBackground :: Configs -> Background -> Player -> Background
updateBackground cfgs back player = back { backXOffset = getOffset playerX windowX xMax
                                         , backYOffset = getOffset playerY windowY yMax
                                         }
    where
        windowX = boardSizeX cfgs
        windowY = boardSizeY cfgs
        xMax = textureWidth $ backArea back
        yMax = textureHeight $ backArea back
        (playerX, playerY) = playerPosition $ player
        getOffset player window areaMax
            | player < (div window 2) = 0
            | player > areaMax - (div window 2) = areaMax - window
            | otherwise = player - (div window 2)

{-
collisionCheck :: GameArea -> Player -> GameArea
collisionCheck gs player =
    case getCollision (bb playerX collideYStart playerWidth collideHeight') cm of
        [] -> gs { gameStatePlayer = player }
        collisions ->
            let newState = foldl updateObject (Right (items, cm, player)) collisions
                (items', cm', player') = case newState of
                    Left (items', cm', player') -> (items', cm', player')
                    Right (items', cm', player') -> (items', cm', player')
            in gs { gameStatePlayer = player', gameStateItemManager = items', collisionMap = cm' }
    where
        collideHeight' = collideHeight $ playerHitBox player
        collideYStart = playerY + playerHeight - collideHeight'
        cm = collisionMap gs
        items = gameStateItemManager gs
        om = collisionObjects gs
        oldPlayer = gameStatePlayer gs
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        playerHeight = textureHeight playerT
        (playerX, playerY) = playerPosition $ player
        updateObject (Right (items, cm, player)) a =
            case om ! a of
                BoardItem -> updateItem (items, cm, player) a
                BoardBarrier ->
                    let ((x, y), tE) = backBarriers (background gs) ! a
                    in Left (items, cm, fixPlayerPosition player x y (textureWidth tE) (textureHeight tE))
        updateObject lGS _ = lGS
        updateItem (items, cm, player) a =
            let item = items ! a
                (x, y, w, h) = getItemDimensions item
                items' = M.adjust (\_ -> item {itemPosition=Nothing}) a items
                cm' = deleteCollision (x, y, w, h, a) cm
                player' = player { playerItems = (M.insertWith (+) (itemType (itemInfo item)) 1 (playerItems player)) }
            in (Right (items', cm', player'))
-}

fixPlayerPosition :: Player -> Int -> Int -> Int -> Int -> Player
fixPlayerPosition player x y w h =
    case getDirection player of
        DUp -> player { playerPosition = (playerX, playerY' - playerHeight) }
        DDown -> player { playerPosition = (playerX, playerY' - playerHeight) }
        DLeft -> player { playerPosition = (playerX', playerY) }
        DRight -> player {playerPosition = (playerX', playerY) }
    where
        playerX' = newPosition playerX (playerX + playerWidth) x (x + w) playerWidth True
        playerY' = newPosition (playerY + playerHeight - 4) (playerY + playerHeight) y (y + h) 4 False
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        playerHeight = textureHeight playerT
        (playerX, playerY) = playerPosition $ player
        newPosition p1 p2 b1 b2 width isX =
            case whichSide p1 p2 b1 b2 of
                LeftOf -> p1
                RightOf -> p1
                LeftOverlap -> if isX then b1 - 1 - width else b1 -  1
                RightOverlap -> if isX then b2 + 1 else b2 + 1 + width
                In -> if p1 - b1 < b2 - p2 then (if isX then b1 - 1 - width else b1 - 1) else (if isX then b2 + 1 else b2 + 1 + width)
                o -> p1
        whichSide playL playR l r = getOrder (playL, playR) (l, r)
