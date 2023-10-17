module GameState.Areas
    ( updateArea
    ) where

import Data.Word (Word32)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Configs
import InputState
import OutputHandles.Types
import GameState.Collision.BoundBox
import GameState.Collision.RTree
import GameState.Player
import GameState.Types
import GameState.Menu.PauseMenu

import Debug.Trace

updateArea :: OutputHandles -> GameConfigs -> InputState -> GameArea -> GameState
updateArea outs cfgs inputs area
    | escapePressed inputs = GameMenu (initPauseMenu outs area) True
    | moveInputPressed inputs && playerStanding (gameStatePlayer area) = GameStateArea area False
    | otherwise = GameStateArea (area'' { background = background' }) True
    where
        player = gameStatePlayer area
        back = background area
        (moved, player') = updatePlayer back inputs player
        area' = (area { gameStatePlayer = player' })
        area'' = if moved then collisionItemCheck area player' else area'
        background' = updateBackground cfgs (background area'') player'


updatePlayer :: Background -> InputState -> Player -> (Bool, Player)
updatePlayer back inputs player@(Player cfgs state) =
    case (inputStateDirection inputs, playerAction state) of
        (Nothing, PlayerStanding _) -> (False, player)
        (Nothing, PlayerMoving (PlayerMove d _ _)) -> (False, player {playerState = state { playerAction = PlayerStanding d}})
        (Just iDir, PlayerStanding _) -> (True, movePlayer back player iDir ts 0 (newPos iDir))
        (Just iDir, PlayerMoving pm@(PlayerMove oldDir oldTs f))
            | iDir == oldDir && (ts - oldTs) > rate -> (True, movePlayer back player iDir ts (mod (f + 1) 8) (newPos iDir))
            | iDir == oldDir ->  (False, player)
            | otherwise -> (True, movePlayer back player iDir ts 0 (newPos iDir))
    where
        ts = inputTimestamp inputs
        newPos newDir = newPosition back player newDir
        rate = stepRate $ playerMoveCfgs cfgs


movePlayer :: Background -> Player -> Direction -> Word32 -> Int -> (Int, Int) -> Player
movePlayer back player@(Player cfg state) dir ts f (newX, newY) = player { playerState = state { playerAction = PlayerMoving (PlayerMove dir ts f), playerPosition = newPos}}
    where
        newPos = foldl movePlayer' (newX, newY) collisions
        (oldX, oldY) = playerPosition state
        hb = getBoundBox dir hitboxes
        hitboxes = playerHitBoxes cfg
        oldPlayerBB = translate oldX oldY hb
        rtree = backCollisions back
        collisions = getIntersections movementBB rtree
        playerT = playerTexture cfg
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
        charSizeX = textureWidth $ playerTexture $ playerCfgs player
        charSizeY = textureHeight $ playerTexture $ playerCfgs player
        moveAmt = moveStep $ playerMoveCfgs $ playerCfgs player
        (xMove, yMove) = updatePosition moveAmt dir
        xMax = (textureWidth $ backArea back) - charSizeX
        yMax = (textureHeight $ backArea back) - charSizeY
        xMin = 0
        yMin = 0
        (x, y) = playerPosition $ playerState player
        x' = x + xMove
        y' = y + yMove
        x'' = max (min x' xMax) xMin
        y'' = max (min y' yMax) yMin

updatePosition :: Int -> Direction -> (Int, Int)
updatePosition m DUp = (0, -m)
updatePosition m DDown = (0, m)
updatePosition m DLeft = (-m, 0)
updatePosition m DRight = (m, 0)

updateNPC :: Background -> Player -> NPCManager -> NPCManager
updateNPC back player npc = 
    where
        dir = getDirection player
        hb = getPlayerHitbox player

targetPosition :: Direction -> BoundBox -> (Int, Int)
targetPosition dir (BB xLeft yUp xRight yDown) =
    case dir of
        DUp -> (xLeft, yDown + 10)
        DDown -> (xLeft, yUp - 10)
        DLeft -> (xRight + 10, yUp)
        DRight -> (xLeft - 10, yUp)


updateBackground :: GameConfigs -> Background -> Player -> Background
updateBackground cfgs back player = back { backXOffset = getOffset playerX windowX xMax
                                         , backYOffset = getOffset playerY windowY yMax
                                         }
    where
        windowX = boardSizeX cfgs
        windowY = boardSizeY cfgs
        xMax = textureWidth $ backArea back
        yMax = textureHeight $ backArea back
        (playerX, playerY) = playerPosition $ playerState player
        getOffset playerPos window areaMax
            | playerPos < (div window 2) = 0
            | playerPos > areaMax - (div window 2) = areaMax - window
            | otherwise = playerPos - (div window 2)


collisionItemCheck :: GameArea -> Player -> GameArea
collisionItemCheck gs player =
    case getCollisionBB hb' cm of
        [] -> gs { gameStatePlayer = player }
        collisions ->
            let (items', cm', player') = foldl updateObject (items, cm, player) collisions
            in gs { gameStatePlayer = player', gameStateItemManager = items', collisionMap = cm' }
    where
        oldPlayer = gameStatePlayer gs
        oldHb = getPlayerHitbox oldPlayer
        hb = getPlayerHitbox player
        hb' = union oldHb hb
        cm = collisionMap gs
        items = gameStateItemManager gs
        updateObject (is, t, p) (ahb, a) =
            let item = is ! a
                items' = M.adjust (\_ -> item {itemPosition=Nothing}) a is
                cm' = delete ahb t
                pState = playerState p
                player' = p { playerState = pState { playerItems = (M.insertWith (+) (itemType (itemInfo item)) 1 (playerItems pState)) }}
            in (items', cm', player')
