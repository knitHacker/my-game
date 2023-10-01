module GameState.Areas
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
import GameState.Menu.PauseMenu

import Debug.Trace

updateArea :: OutputHandles -> Configs -> InputState -> GameArea -> GameState
updateArea outs cfgs inputs area
    | escapePressed inputs = GameMenu (initPauseMenu outs area) True
    | moveInputPressed inputs && playerStanding (gameStatePlayer area) = GameStateArea area False
    | otherwise = GameStateArea (area' { background = background' }) True
    where
        player = gameStatePlayer area
        back = background area
        (moved, player') = updatePlayer back inputs player
        area' = (area { gameStatePlayer = player' })
        area'' = undefined -- if moved then collisionCheck area player' else area'
        background' = updateBackground cfgs (background area') player'


updatePlayer :: Background -> InputState -> Player -> (Bool, Player)
updatePlayer back inputs player@(Player _ hb pos mv _ cfgs) =
    case (inputStateDirection inputs, mv) of
        (Nothing, Left _) -> (False, player)
        (Nothing, Right (PlayerMove d _ _)) -> (False, player { playerMovement = Left d})
        (Just iDir, Left _) -> (True, movePlayer back player iDir ts 0 (newPos iDir))
        (Just iDir, Right pm@(PlayerMove oldDir oldTs f))
            | iDir == oldDir && (ts - oldTs) > rate -> (True, movePlayer back player iDir ts (mod (f + 1) 8) (newPos iDir))
            | iDir == oldDir ->  (False, player)
            | otherwise -> (True, movePlayer back player iDir ts 0 (newPos iDir))
    where
        ts = inputTimestamp inputs
        newPos newDir = newPosition back player newDir
        rate = stepRate $ playerCfgs player


movePlayer :: Background -> Player -> Direction -> Word32 -> Int -> (Int, Int) -> Player
movePlayer back player dir ts f (newX, newY) = player { playerMovement = Right (PlayerMove dir ts f), playerPosition = newPos}
    where
        newPos = foldl movePlayer' (newX, newY) collisions
        (oldX, oldY) = playerPosition player
        hb = getBoundBox dir hitboxes
        hitboxes = playerHitBoxes player
        oldPlayerBB = translate oldX oldY hb
        rtree = backCollisions back
        collisions = getIntersections movementBB rtree
        playerT = playerTexture player
        playerWidth = textureWidth playerT
        playerBB = translate newX newY hb
        movementBB = union oldPlayerBB playerBB
        movePlayer' (x, y) b@(BB x1 y1 x2 y2) =
            case dir of
                DUp -> (x, y + (y2 - y1))
                DDown -> (x, y - (y2 - y1))
                DLeft -> (x + (x2 - x1), y)
                DRight -> (x - (x2 - x1), y)

newPosition :: Background -> Player -> Direction -> (Int, Int)
newPosition back player dir = (x'', y'')
    where
        charSizeX = textureWidth (playerTexture player)
        charSizeY = textureHeight (playerTexture player)
        moveAmt = moveStep $ playerCfgs player
        (xMove, yMove) = updatePosition moveAmt dir
        xMax = (textureWidth $ backArea back) - charSizeX
        yMax = (textureHeight $ backArea back) - charSizeY
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

-}
