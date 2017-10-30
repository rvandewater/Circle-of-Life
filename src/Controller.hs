-- | This module defines how the state changes
--   in response to time and user input
{-# language NamedFieldPuns #-}
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState {paused, player = player@Player {pos, hitbox, fireRate, bullet }, keys, lost, bullets, enemies, randomGen, score}=
    if not paused then return $ gstate { 
                                          elapsedTime = elapsedTime gstate + secs, 
                                          player      = plr {pos = movementUpdate keys pos, lastFire = fire, health = health - plrcolldamage},
                                          bullets     = map bulletAniUp (filter boundcheck (map bulletUpdate bulletsover)) , 
                                          lost        = (health <= 0), 
                                          enemies     = enemycollover,
                                          randomGen   = nrg,
                                          score       = score + killscore
                                          }
                  else return   gstate
        where (bulletlist, fire)                                  = shootUpdate secs gstate
              bullenem                                            = map (enemyShoot secs)  enemies
              enemsaftershoot                                     = map snd bullenem
              bulletsaftenem                                      = map fromJust (filter isJust (map fst bullenem))                                   -- Checks and fires if player can
              bulletsshot                                         = (bulletlist ++ bulletsaftenem)                
              (bulletsover, plr@Player {health} )                 = shipHit bulletsshot player                                     -- Checks if the ship hits one of the bullets
              (enemiesover, bulletsafterenemies )                 = shipsHit bulletsover enemsaftershoot                                  -- Checks if one of the ships hits one of the bullets
              boundcheck (Bullet pos (BulletType speed box dmg _) _ ) = not (outOfBounds pos box)                                 -- Checks if the bullet is not out of bounds
              killscore                                           = sum (map enemyScore enemiesover)                              -- Adds score to player score if enemy died
              (updatedenemies, nrg)                               = enemyUpdate gstate (mapMaybe enemyKill enemiesover)         -- Updates the killed enemies after collision check
              enemycollover                                       = map snd collisioncheck                                        -- Enemies after collision check
              plrcolldamage                                       = sum (map fst collisioncheck)                                  -- Damage done to the player in collision check
              collisioncheck                                      = (map (enemyColl player) updatedenemies)                          -- Collision check of enemies
              bulletAniUp bullet@Bullet{frame}                    | frame < 2 = bullet {frame = frame + secs}                     -- Bullet animation frame update
                                                                  | otherwise = bullet {frame = 0}
              
              
enemyKill :: Enemy -> Maybe Enemy
enemyKill enemy@Enemy{ehealth}                            | ehealth >= 0 = Just enemy
                                                          | otherwise = Nothing
enemyScore :: Enemy -> Int
enemyScore enemy@Enemy{ehealth, killpoints}               | ehealth >= 0 = 0
                                                          | otherwise = killpoints

enemyUpdate :: GameState -> [Enemy] -> ([Enemy],StdGen)
enemyUpdate gs@GameState{randomGen} enemies | isJust newEn = ((fromJust newEn) : (map positionUpdate enemies),rg)
                                            | otherwise    = (map positionUpdate enemies,rg)
                                      where positionUpdate en@Enemy{epos, ehitbox}  = en{epos = updatePosE (aiMove en gs) epos ehitbox}
                                            (newEn, rg)    = newEnemy randomGen
enemyCanShoot :: Enemy -> Bool                                            
enemyCanShoot Enemy{efireRate, eLastFire} | efireRate < eLastFire = True
                                          | otherwise = False

enemyShoot ::  Float -> Enemy -> (Maybe Bullet,Enemy)
enemyShoot secs enem@Enemy{eBullet, eLastFire, epos = (Position xp yp), ehitbox= (HitBox x y)} | enemyCanShoot enem = (Just (Bullet (Position xp (yp- (fromIntegral y) `div` 2 )) eBullet 0) , enem{ eLastFire = 0}  )
                                                                                               | otherwise = (Nothing,  enem{ eLastFire = secs + eLastFire})
aiMove :: Enemy -> GameState -> Move
aiMove Enemy{epos = (Position ex ey), ehitbox = (HitBox width height), eai, espeed} GameState{player = Player{pos = (Position px py)}, bullets}   
    | eai == 0  = Move (toPlayer ex px) (toPlayer ey py)                            --trail
    | eai == 1  = Move (toPlayer ex px) (-espeed)                                   --trail on x
    | eai == 2  = Move dodgeBullet      (-espeed)                                   --dodge
    | eai == 3  
        = if dodgeBullet == 0 
            then  Move (toPlayer ex px) (toPlayer ey py)                            --dodge and trail
            else  Move dodgeBullet      (-espeed)
    | otherwise = Move 0                (-espeed)                                   --stationary

  where toPlayer ex px  | ex < px - 1 = espeed
                        | ex > px + 1 = -espeed - espeed
                        | otherwise = 0

        dodgeBullet     | null tododge = 0
                        | otherwise    = 2*(fromJust (head tododge))

        zone bullet@Bullet{position = (Position x y)} 
                        | reaction y && inPath x = if ex < x then Just (-espeed) else Just espeed
                        | otherwise              = Nothing             

        tododge         = filter isJust (map zone bullets)
        reaction y      = y < ey + 200 && y > ey - 200
        inPath x        = x > (ex - width - 20) && x < (ex + width + 20)


newEnemy :: StdGen -> (Maybe Enemy,StdGen) 
newEnemy rg | number == 0 = (Just (selectEnemy!!ai) {epos = Position location 550},rg3)
            | otherwise   = (Nothing,rg3)
            where (number,    rg1)  = randomR (0,     100 :: Int) rg            --maybe a new enemy spawns
                  (location,  rg2)  = randomR (-275,  275 :: Int) rg1           --the new enemy location is random
                  (ai,        rg3)  = randomR (0,       4 :: Int) rg2           --the new enemy has a random ai assigned                            

selectEnemy :: [Enemy]
selectEnemy = [Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color blue    (rectangleSolid 50 50)) 0 2 100
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color white   (rectangleSolid 50 50)) 1 2 50
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color yellow  (rectangleSolid 50 50)) 2 2 10
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color red     (rectangleSolid 50 50)) 3 2 10
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color black   (rectangleSolid 50 50)) 4 2 10]


shipsHit :: Ship k => [Bullet] -> [k] -> ([k],[Bullet] )
shipsHit bullets = foldl oneship ( [], bullets)                                  where
                                    oneship :: Ship k => ([k],[Bullet]) -> k -> ([k],[Bullet])
                                    oneship (shiplist, bullets') ship = (ship':shiplist, bull) 
                                        where (bull, ship')= shipHit  bullets' ship 
                                      
shipHit :: Ship k => [Bullet] -> k -> ([Bullet], k)
shipHit bullets k 
  =   ( (filter checker bullets),getDamage totaldam k) 
          where totaldam = (sum (mapMaybe (isHit k) bullets  ))
                checker (thisBull@(Bullet pos (BulletType speed size bulletDamage _ ) _)) = (isNothing(isHit k thisBull))

bulletUpdate :: Bullet -> Bullet
bulletUpdate (Bullet pos (BulletType speed box dmg pic) up) = (Bullet (updatePos speed pos) (BulletType speed box dmg pic) up)

shootUpdate :: Float -> GameState -> ([Bullet],Float)
shootUpdate secs gstate@GameState { elapsedTime, bullets, player = player@Player {pos = Position {xpos,ypos}, hitbox, fireRate, bullet, lastFire }, keys = keys@KeysPressed {space}} 
  | canshoot = ((Bullet (Position xpos (ypos + 20)) bullet 0): bullets, 0 ) 
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

