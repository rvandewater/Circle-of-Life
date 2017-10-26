-- | This module defines how the state changes
--   in response to time and user input
{-# language NamedFieldPuns #-}
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe

-- | Handle one iteration of the game
step :: StdGen -> Float -> GameState -> IO GameState
step rg secs gstate@GameState {paused, player = player@Player {pos, hitbox, fireRate, bullet, health }, keys, lost, bullets, enemies}=
    if not paused then return $ gstate { 
                                          elapsedTime = elapsedTime gstate + secs, 
                                          player      = plr {pos         =  movementUpdate keys pos
                                                                , lastFire  = fire
                                                                },
                                          bullets     =  map bulletUpdate bulletsafterenemies, 
                                          lost        = (health <= 0), 
                                          enemies     = map enemyUpdate (filter enemykill enemiesover)}
                  else return gstate
        where (bulletlist , fire)         = shootUpdate secs gstate
              (bulletsover , plr )        = (shipHit bulletlist player) 
              (enemiesover, bulletsafterenemies ) = (shipsHit bulletsover enemies)
              enemykill (Enemy {ehealth}) = ehealth >= 0

enemyUpdate :: Enemy -> Enemy
enemyUpdate enemy@Enemy{epos, ehitbox} = enemy {epos = updatePosE (Move 0 (-2)) epos ehitbox}

newEnemies :: StdGen -> ([Enemy],StdGen)
newEnemies rg = if chance then ([(Enemy beginPos eHitBox 0 standardBullet 0 100)],rg) else ([],rg)
                        where chance = False --random (range 0 10) rg

shipsHit :: Ship k => [Bullet] -> [k] -> ([k],[Bullet] )
shipsHit bullets ships = foldl oneship ( [], bullets) ships
                                  where
                                    oneship :: Ship k => ([k],[Bullet]) -> k -> ([k],[Bullet])
                                    oneship (shiplist, bullets') ship = (ship':shiplist, bull) 
                                        where (bull, ship')= shipHit  bullets' ship 
                                      
shipHit :: Ship k => [Bullet] -> k -> ([Bullet], k)
shipHit bullets k 
  =   ( (filter checker bullets),getDamage totaldam k) 
          where totaldam = (sum (mapMaybe (isHit k) bullets  ))
                checker (thisBull@(Bullet pos (BulletType speed size bulletDamage ))) = (not(outOfBounds pos size) && isNothing(isHit k thisBull))

bulletUpdate :: Bullet -> Bullet
bulletUpdate (Bullet pos (BulletType speed box dmg) ) = (Bullet (updatePos speed pos) (BulletType speed box dmg))

shootUpdate :: Float -> GameState -> ([Bullet],Float)
shootUpdate secs gstate@GameState { elapsedTime, bullets, player = player@Player {pos = Position {xpos,ypos}, hitbox, fireRate, bullet, lastFire }, keys = keys@KeysPressed {space}} 
  | canshoot = ((Bullet (Position xpos (ypos + 20)) bullet): bullets, 0 ) 
  | otherwise = (bullets, lastFire + secs )
    where canshoot = space && (lastFire> fireRate)
 
                                                         

-- | Updating Position depending on keys pressed
movementUpdate :: KeysPressed -> Position -> Position
movementUpdate (KeysPressed w a s d sp) =   boolupdate w (Move 0 movementSpeed). 
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
inputKey (EventKey (Char 'p') Down _ _) gstate@GameState { keys, paused }= gstate {paused = not paused}
--Handle game input
inputKey (EventKey c _ _ _ ) gstate@GameState { keys, paused } = gstate {keys = (updatePress c keys )}
-- Otherwise keep the same 
inputKey _ gstate = gstate 

--Updates the KeyPressed data type to reflect current pressed keys.
updatePress :: Key -> KeysPressed -> KeysPressed
updatePress (Char 'w')            keys@KeysPressed {w}     = keys { w     = not w }
updatePress (Char 'a')            keys@KeysPressed {a}     = keys { a     = not a }
updatePress (Char 's')            keys@KeysPressed {s}     = keys { s     = not s }
updatePress (Char 'd')            keys@KeysPressed {d}     = keys { d     = not d }
updatePress (SpecialKey KeySpace) keys@KeysPressed {space} = keys { space = not space}
updatePress _                     x                          = x

