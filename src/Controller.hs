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
step secs gstate@GameState { screen}                                                     | screen == PausedGame || screen == MainMenu || screen == GameOver  = return gstate
                                                                                         | otherwise = gameUpdate secs gstate

gameUpdate :: Float -> GameState -> IO GameState
gameUpdate secs gstate@GameState {player = player@Player {pos, hitbox, fireRate, bullet }, screen, keys, bullets, enemies, randomGen, score} =
     return $ gstate { 
                                          elapsedTime = elapsedTime gstate + secs, 
                                          player      = plr {pos = movementUpdate keys pos, lastFire = fire, health = health - plrcolldamage},
                                          bullets     = map bulletAniUp (filter boundcheck (map bulletUpdate bulletsover)) , 
                                          screen      = screenChecker screen, 
                                          enemies     = enemycollover,
                                          randomGen   = nrg,
                                          score       = score + killscore
                                          }
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
              screenChecker      scr                             | health <= 0 = GameOver
                                                                 | otherwise = scr
              
              
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

{-                                                                                               aiMove :: Enemy -> GameState -> Move
aiMove e@Enemy{eai} gs  | eai == 0  = trail      e
                        | eai == 1  = aim        e
                        | eai == 2  = dodge      e
                        | eai == 3  = dodgetrail e
                        | otherwise = stationary e

trail :: Enemy -> GameState -> Move
trail Enemy{epos = (Position ex ey), ehitbox = (HitBox width height), eai, espeed} GameState{player = Player{pos = (Position px py)}, bullets}
 = Move (toPlayer ex px) (toPlayer ey py) 
      where toPlayer ex px    | ex < px - 1 = espeed
                              | ex > px + 1 = -espeed - espeed
                              | otherwise = 0
-}
aiMove :: Enemy -> GameState -> Move
aiMove Enemy{epos = (Position ex ey), ehitbox = (HitBox width height), eai, espeed} GameState{player = Player{pos = (Position px py)}, bullets}   
    | eai == 0  = Move (toPlayer ex px) (toPlayer ey py)                            --trail
    | eai == 1  = Move (toPlayer ex px) (-espeed)                                   --trail on x
    | eai == 2  = Move dodgeBullet      (-espeed)                                   --dodge
    | eai == 3  = if dodgeBullet == 0 
            then  Move (toPlayer ex px) (toPlayer ey py)                            --dodge and trail
            else  Move dodgeBullet      (-espeed)
    
    | otherwise = Move 0                (-espeed)                                   --stationary

  where toPlayer ex px  | ex < px - 1 = espeed
                        | ex > px + 1 = -espeed - espeed
                        | otherwise = 0

        dodgeBullet     | null tododge = 0
                        | otherwise    = 2*(fromJust (head tododge))     

        tododge         = filter isJust (map zone bullets)

        zone Bullet{position = (Position x y), kind = BulletType{speed = Move xmov ymov}}
            | abs ey -  abs y < 200                                           --if in range y
              && (ymov > 0 && ey > y) || (ymov < 0 && ey < y)                 --if closing in on y
              && (abs x < abs ex + width + 20)                                --if closing in on x 
              = if ex < x     then Just (-espeed)
                              else Just espeed
            | otherwise       =    Nothing

newEnemy :: StdGen -> (Maybe Enemy,StdGen) 
newEnemy rg | number == 0 = (Just (selectEnemy!!ai) {epos = Position location 550},rg3)
            | otherwise   = (Nothing,rg3)
            where (number,    rg1)  = randomR (0,     100 :: Int) rg            --maybe a new enemy spawns
                  (location,  rg2)  = randomR (-275,  275 :: Int) rg1           --the new enemy location is random
                  (ai,        rg3)  = randomR (0,       4 :: Int) rg2           --the new enemy has a random ai assigned                            

selectEnemy :: [Enemy]
selectEnemy = [Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color blue    (rectangleSolid 50 50)) 0 2 10
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color white   (rectangleSolid 50 50)) 1 2 20
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color yellow  (rectangleSolid 50 50)) 2 2 30
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color red     (rectangleSolid 50 50)) 3 2 100
              ,Enemy enemySpawn eHitBox 1 standardEBullet 0 5 (color black   (rectangleSolid 50 50)) 4 2 1]


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
input e gstate@GameState{screen = MainMenu}  = return (mainMenuUpdate e gstate)
input e gstate@GameState{screen = PlayGame}  = return (playGameUpdate e gstate)
input e gstate@GameState{screen = PausedGame}  = return (pausedGameUpdate e gstate)
input e gstate@GameState{screen = GameOver} = return (gameOverUpdate e gstate)
input e gstate@game  = return gstate

pausedGameUpdate :: Event -> GameState -> GameState
pausedGameUpdate (EventKey (Char 'p')  Down _ _) gstate@GameState{screen} = gstate{screen = PlayGame} 
pausedGameUpdate _ gstate = gstate

-- | Handle input of a key
playGameUpdate :: Event -> GameState -> GameState
--Pause button handle
playGameUpdate (EventKey (Char 'p') Down _ _) gstate@GameState { screen} = gstate{screen = PausedGame}
--Reset game

--Handle game input
playGameUpdate (EventKey c Up _ _ ) gstate@GameState { keys} = gstate {keys = (updatePress c False keys )}
playGameUpdate (EventKey c Down _ _ ) gstate@GameState { keys } = gstate {keys = (updatePress c True keys )}
-- Otherwise keep the same 
playGameUpdate _ gstate = gstate 

--Updates the KeyPressed data type to reflect current pressed keys.
updatePress :: Key -> Bool -> KeysPressed -> KeysPressed
updatePress (Char 'w')  b                          keys     = keys { w     = b }
updatePress (Char 'a')  b          keys    = keys { a     = b }
updatePress (Char 's')  b          keys     = keys { s     = b }
updatePress (Char 'd')  b          keys    = keys { d     = b }
updatePress (SpecialKey KeySpace) b keys = keys { space = b }
updatePress _       _              x                          = x

mainMenuUpdate :: Event -> GameState -> GameState
mainMenuUpdate (EventKey (SpecialKey KeySpace) Up _ _) gstate@GameState{screen} = gstate{screen = PlayGame}
mainMenuUpdate _ gstate = gstate 

gameOverUpdate:: Event -> GameState -> GameState
gameOverUpdate (EventKey (Char 'r') Down _ _) GameState{randomGen} = initialState randomGen
gameOverUpdate _ gstate = gstate 