-- | This module defines how the state changes
--   in response to time and user input
{-# language NamedFieldPuns #-}
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState { paused, bullets, player, keys })=
    if (not paused) then return $ gstate { 
    elapsedTime = elapsedTime gstate + secs, 
    player = Player (movementUpdate keys (getposition player)) (gethitbox player),
    bullets = bulletListUpdate (shootUpdate keys (getposition player) bullets)}
    else return gstate
      where getposition (Player x _) = x
            gethitbox (Player _ y ) = y
         

bulletListUpdate :: [Bullet] -> [Bullet]
bulletListUpdate bullets = map bulletUpdate (filter (\(Bullet pos speed size damage) -> not(outOfBounds pos size)) bullets)

bulletUpdate :: Bullet -> Bullet
bulletUpdate (Bullet pos speed size damage) = Bullet (updatePos speed pos) speed size damage

shootUpdate :: KeysPressed -> Position -> [Bullet] -> [Bullet]
shootUpdate (KeysPressed w a s d sp) playerpos bullets = if sp 
                                                         then ((Bullet playerpos bulletSpeed bulletHitBox bulletDamage):bullets) 
                                                         else bullets

-- | Updating Position depending on keys pressed
movementUpdate :: KeysPressed -> Position -> Position
movementUpdate (KeysPressed w a s d sp) =  boolupdate w (Move 0 movementSpeed). 
                                      boolupdate a (Move (- movementSpeed) 0).
                                      boolupdate s (Move 0 (- movementSpeed)).
                                      boolupdate d (Move movementSpeed 0)
                                        where boolupdate True mov = updatePosP mov
                                              boolupdate False _ = id

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | Handle input of a key
inputKey :: Event -> GameState -> GameState
--Pause button handle
inputKey (EventKey (Char 'p') Down _ _) gstate@(GameState { keys, paused })= gstate {paused = not paused}
--Handle game input
inputKey (EventKey c _ _ _ ) gstate@(GameState { keys, paused }) = gstate {keys = (updatePress c keys )}
-- Otherwise keep the same 
inputKey _ gstate = gstate 

--Updates the KeyPressed data type to reflect current pressed keys.
updatePress :: Key -> KeysPressed -> KeysPressed
updatePress (Char 'w')            keys@(KeysPressed {w})     = keys { w     = not w }
updatePress (Char 'a')            keys@(KeysPressed {a})     = keys { a     = not a }
updatePress (Char 's')            keys@(KeysPressed {s})     = keys { s     = not s }
updatePress (Char 'd')            keys@(KeysPressed {d})     = keys { d     = not d }
updatePress (SpecialKey KeySpace) keys@(KeysPressed {space}) = keys { space = not space}
updatePress _                     x                          = x

