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
step secs gstate@(GameState { paused, bullets, playerPos, keys })
  = -- Just update the elapsed time
   if (not paused) then return $ gstate { 
    elapsedTime = elapsedTime gstate + secs, 
    playerPos = (movementUpdate (keys) (playerPos)),
    bullets = map bulletUpdate (shootUpdate keys playerPos bullets)}
   else return gstate

bulletUpdate :: Bullet -> Bullet
bulletUpdate (Bullet pos speed size damage) = Bullet (updatePos speed pos) speed size damage

shootUpdate :: KeysPressed -> Position -> [Bullet] -> [Bullet]
shootUpdate (KeysPressed w a s d sp) playerpos bullets = if sp then ((Bullet playerpos bulletSpeed bulletBox bulletDamage):bullets) else bullets

movementUpdate :: KeysPressed -> Position -> Position
movementUpdate (KeysPressed w a s d sp) =  boolupdate w (Move 0 movementSpeed). 
                                      boolupdate a (Move (- movementSpeed) 0).
                                      boolupdate s (Move 0 (- movementSpeed)).
                                      boolupdate d (Move movementSpeed 0)
                              where boolupdate True mov = updatePos mov
                                    boolupdate False _ = id
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
{-inputKey (EventKey (Char c) _ _ _) gstate@(GameState {playerPos})
   | c == 'w' = gstate { playerPos = (updatePos playerPos (Move 0 movementSpeed))  }
   | c == 'a' = gstate { playerPos = Position (curposx-10) curposy  }
   | c == 's' = gstate { playerPos = Position curposx (curposy-10)  }
   | c == 'd' = gstate { playerPos = Position (curposx + 10) curposy  }
     where xpos (Position x y) = x
           ypos (Position x y) = y
           curposx = xpos playerPos
           curposy = ypos playerPos -}
inputKey (EventKey (Char 'p') Down _ _) gstate@(GameState { keys, paused })= gstate {paused = not paused}
inputKey (EventKey c _ _ _ ) gstate@(GameState { keys, paused }) = gstate {keys = (updatePress c keys )}
{-
inputKey (EventKey  _ _ _ _) gstate@(GameState {playerPos}) = 
  gstate { playerPos = (updatePos playerPos (Move 0 movementSpeed))  -}
    -- If the user presses a specific character key, move the player

inputKey _ gstate = gstate -- Otherwise keep the same 

updatePress :: Key -> KeysPressed -> KeysPressed
updatePress (Char 'w') keys@(KeysPressed {w}) = keys { w = not w }
updatePress (Char 'a') keys@(KeysPressed {a}) = keys { a = not a }
updatePress (Char 's') keys@(KeysPressed {s}) = keys { s = not s }
updatePress (Char 'd') keys@(KeysPressed {d}) = keys { d = not d }
updatePress (SpecialKey KeySpace) keys@(KeysPressed {space}) = keys { space = not space}

updatePress _ x = x

