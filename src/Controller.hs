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
step secs gstate@GameState { screen, score}                                                     | screen == PausedGame || screen == MainMenu || screen == DifficultySelect || screen == LevelSelect || screen==GameOver || screen == HighScores = return gstate
                                                                                                | screen == WriteScore = 
                                                                                                                        do  
                                                                                                                            appendFile "highscore"  ((show score)++ " ") 
                                                                                                                            return gstate{screen = GameOver}
                                                                                                | screen == ReadScore =  do scores <- readFile "highscore"
                                                                                                                            return gstate {scorelist = scores, screen = HighScores}
                                                                                                | otherwise = gameUpdate secs gstate

gameUpdate :: Float -> GameState -> IO GameState
gameUpdate secs gstate@GameState {player = player@Player {pos, hitbox, fireRate, bullet}, screen, keys, bullets, enemies, randomGen, score} =
     return $ gstate { 
                                          elapsedTime = elapsedTime gstate + secs, 
                                          player      = plr {pos = movementUpdate keys pos, lastFire = fire, health = health - plrcolldamage, hitAnim = hitAnimReset hitAnim},
                                          bullets     = map bulletAniUp (filter boundcheck (map bulletUpdate bulletsafterenemies)) , 
                                          screen      = screenChecker screen, 
                                          enemies     = map enemAnimReset(enemycollover),
                                          randomGen   = nrg,
                                          score       = score + killscore
                                          }
        where (bulletlist, fire)                                  = shootUpdate secs gstate
              bullenem                                            = map (enemyShoot secs)  enemies
              enemsaftershoot                                     = map snd bullenem
              bulletsaftenem                                      = map fromJust (filter isJust (map fst bullenem))                                   -- Checks and fires if player can
              bulletsshot                                         = (bulletlist ++ bulletsaftenem)                
              (bulletsover, plr@Player {health, hitAnim} )        = shipHit bulletsshot player                                     -- Checks if the ship hits one of the bullets
              (enemiesover, bulletsafterenemies )                 = shipsHit bulletsover enemsaftershoot                                  -- Checks if one of the ships hits one of the bullets
              boundcheck (Bullet pos (BulletType speed box dmg _) _ ) = not (outOfBounds pos box)                                 -- Checks if the bullet is not out of bounds
              killscore                                           = sum (map enemyScore enemiesover)                              -- Adds score to player score if enemy died
              (updatedenemies, nrg)                               = enemyUpdate gstate (mapMaybe enemyKill enemiesover)         -- Updates the killed enemies after collision check
              enemycollover                                       = map snd collisioncheck                                        -- Enemies after collision check
              plrcolldamage                                       = sum (map fst collisioncheck)                                  -- Damage done to the player in collision check
              collisioncheck                                      = (map (enemyColl player) updatedenemies)                          -- Collision check of enemies
              bulletAniUp bullet@Bullet{frame}                    | frame < 2 = bullet {frame = frame + secs}                     -- Bullet animation frame update
                                                                  | otherwise = bullet {frame = 0}
              screenChecker      scr                             | health <= 0 = WriteScore
                                                                 | otherwise = scr
              hitAnimReset               frm                     | plrcolldamage > 0 = 1
                                                                 | frm < 0 = 0
                                                                 | otherwise = frm - secs
              enemAnimReset  enem@Enemy{eHitAnim, killAnim}      | killAnim > 0 = enem{killAnim= killAnim - secs}
                                                                 | eHitAnim > 0 = enem{eHitAnim = eHitAnim-secs}
                                                                 | eHitAnim <= 0 = enem{eHitAnim = 0}
              
              
              


enemyUpdate :: GameState -> [Enemy] -> ([Enemy],StdGen)
enemyUpdate gs enemies  | isJust newEn = ((fromJust newEn) : (map positionUpdate enemies),rg)
                        | otherwise    = (map positionUpdate enemies,rg)
                              where positionUpdate en@Enemy{epos, ehitbox}  = en{epos = updatePosE (aiMove en gs) epos ehitbox}
                                    (newEn, rg)    = newEnemy gs

aiMove :: Enemy -> GameState -> Move
aiMove enemy@Enemy{epos = (Position ex ey), eai, espeed} GameState{player = Player{pos = (Position px py)}, bullets}   
    | eai == 0  = Move (toPlayer ex px espeed)      (toPlayer ey py espeed)              --trail
    | eai == 1  = Move (toPlayer ex px espeed)      (-espeed)                            --aim (trail on x only)
    | eai == 2  = Move (dodgeBullet bullets enemy)  (-espeed)                            --dodge
    | eai == 3  = if (dodgeBullet bullets enemy) == 0                                    --dodge and trail
            then  Move (toPlayer ex px espeed)      (toPlayer ey py espeed)              
            else  Move (dodgeBullet bullets enemy)  (-espeed)
    | otherwise = Move 0                            (-espeed)                            --stationary

dodgeBullet :: [Bullet] -> Enemy -> Int
dodgeBullet bullets Enemy{epos = (Position ex ey), ehitbox = (HitBox width height), espeed}  
                            | null tododge = 0
                            | otherwise    = 2*(fromJust (head tododge))

        where   tododge     = filter isJust (map zone bullets)
                zone Bullet{position = (Position x y), kind = BulletType{speed = Move xmov ymov}}
                            | abs ey -  abs y < 200                                         --if in range y
                            && (ymov > 0 && ey > y) || (ymov < 0 && ey < y)                 --if closing in on y
                            && (abs x < abs ex + width + 20)                                --if closing in on x 
                            = if ex < x     then Just (-espeed)
                                            else Just espeed
                            | otherwise       =    Nothing

toPlayer :: Int -> Int -> Int -> Int
toPlayer ex px espeed | ex < px - 1 = espeed
                      | ex > px + 1 = -espeed - espeed
                      | otherwise = 0

newEnemy :: GameState -> (Maybe Enemy,StdGen) 
newEnemy gs@GameState{difficulty, randomGen}    | number == 0 = (Just (selectEnemy difficulty enemytype) {epos = Position location 550},rg3)
                                                | otherwise   = (Nothing,rg3)
            where (number,    rg1)  = randomR (0,     100 :: Int) randomGen     --maybe a new enemy spawns
                  (location,  rg2)  = randomR (-275,  275 :: Int) rg1           --the new enemy location is random
                  (enemytype, rg3)  = randomR (0,       0 :: Int) rg2           --the new enemy has a random type assigned                            


bulletUpdate :: Bullet -> Bullet
bulletUpdate (Bullet pos (BulletType speed box dmg pic) up) = (Bullet (updatePos speed pos) (BulletType speed box dmg pic) up)

shootUpdate :: Float -> GameState -> ([Bullet],Float)
shootUpdate secs gstate@GameState { elapsedTime, bullets, player = player@Player {pos = Position {xpos,ypos}, hitbox, fireRate, bullet= thisbull@BulletType{size}, lastFire }, keys = keys@KeysPressed {space}} 
  | canshoot = ((Bullet (Position xpos (ypos + (sizer hitbox) +(sizer size) )) thisbull 0): bullets, 0 ) 
  | otherwise = (bullets, lastFire + secs )
    where canshoot = space && (lastFire> fireRate)
          sizer (HitBox xbox ybox) = ybox `quot` 2
                                                         

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
input e gstate@GameState{screen = DifficultySelect } = return (difficultyUpdate e gstate)
input e gstate@GameState{screen = LevelSelect } = return (levelUpdate e gstate)
input e gstate@GameState{screen = PlayGame}  = return (playGameUpdate e gstate)
input e gstate@GameState{screen = PausedGame}  = return (pausedGameUpdate e gstate)
input e gstate@GameState{screen = GameOver} = return (gameOverUpdate e gstate)
input e gstate@GameState{screen = HighScores} = return (highScoreUpdate e gstate)
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
updatePress (Char 'w')  b           keys = keys { w     = b }
updatePress (Char 'a')  b           keys = keys { a     = b }
updatePress (Char 's')  b           keys = keys { s     = b }
updatePress (Char 'd')  b           keys = keys { d     = b }
updatePress (SpecialKey KeySpace) b keys = keys { space = b }
updatePress _                     _ x    = x

mainMenuUpdate :: Event -> GameState -> GameState
mainMenuUpdate (EventKey (SpecialKey KeySpace) Up _ _) gstate@GameState{screen} = gstate{screen = DifficultySelect}
mainMenuUpdate (EventKey (SpecialKey KeyEnter) Up _ _)  gstate@GameState{screen} = gstate{screen = ReadScore}
mainMenuUpdate _ gstate = gstate 

gameOverUpdate :: Event -> GameState -> GameState
gameOverUpdate (EventKey (Char 'r') Down _ _) GameState{randomGen} = initialState randomGen
gameOverUpdate _ gstate = gstate 

difficultyUpdate :: Event -> GameState -> GameState
difficultyUpdate (EventKey (Char '1') Down _ _) gstate@GameState{screen} = gstate{screen = LevelSelect, difficulty = 1}
difficultyUpdate (EventKey (Char '2') Down _ _) gstate@GameState{screen} = gstate{screen = LevelSelect, difficulty = 2}
difficultyUpdate (EventKey (Char '3') Down _ _) gstate@GameState{screen} = gstate{screen = LevelSelect, difficulty = 3}
difficultyUpdate _ gstate = gstate

levelUpdate :: Event -> GameState -> GameState
levelUpdate (EventKey (Char '1') Down _ _) gstate@GameState{screen} = gstate{screen = PlayGame, level = 1}
levelUpdate (EventKey (Char '2') Down _ _) gstate@GameState{screen} = gstate{screen = PlayGame, level = 2}
levelUpdate (EventKey (Char '3') Down _ _) gstate@GameState{screen} = gstate{screen = PlayGame, level = 3}
levelUpdate _ gstate = gstate

highScoreUpdate :: Event -> GameState -> GameState
highScoreUpdate (EventKey (SpecialKey KeyEnter) Up _ _)  gstate@GameState{screen} = gstate{screen = MainMenu}
highScoreUpdate _ gstate = gstate 