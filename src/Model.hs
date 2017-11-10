-- | This module contains the data types
--   which represent the state of the game
{-# language NamedFieldPuns #-}
module Model where

import Graphics.Gloss
import System.Random
import Data.Maybe
import Data.List

-- ********************* DATA TYPES ***************
data GameState = GameState {
                   elapsedTime :: Float
                 , screen :: GameScreen
                 , player :: Player
                 , keys :: KeysPressed
                 , bullets :: [Bullet]
                 , enemies :: [Enemy]
                 , level :: Int
                 , difficulty :: Int
                 , score :: Int
                 , plrname :: String
                 , scorelist :: String
                 , randomGen :: StdGen
                  }


data Bullet = Bullet { position:: Position, kind :: BulletType, frame :: Float}

data GameScreen = MainMenu | GameOver| WriteScore Bool | ReadScore | PlayGame | DifficultySelect | LevelSelect | PausedGame | HighScores | NoScreen 
  deriving (Eq)

data BulletType =  BulletType { speed:: Move, size :: HitBox, damage :: Float, bulletpic :: Picture}

data HitBox = HitBox { width :: Int, height :: Int}   

data Player = Player { pos :: Position, hitbox :: HitBox, fireRate :: Float, bullet :: BulletType, lastFire :: Float, health :: Float, hitAnim :: Float, invincibility :: Float }

data Enemy = Enemy { epos :: Position, ehitbox :: HitBox, efireRate :: Float, eBullet :: BulletType, eLastFire :: Float, ehealth :: Float , model :: Int, eai :: Int, espeed :: Int, killpoints :: Int, eHitAnim :: Float, killAnim :: Float }

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool, space :: Bool }


-- ********************* FUNCTIONS ***************
-- | update position
updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = Position (xpos + xmov) (ypos + ymov)

-- | update position
updatePosE ::  Move -> Position -> HitBox -> Position
updatePosE  (Move xmov ymov) (Position xpos ypos) ehitbox = if validPosition then Position newX newY else Position xpos newY
      where validPosition :: Bool
            validPosition = not (outOfBounds (Position newX 0) ehitbox)
            newX          = xpos + xmov
            newY          = ypos + ymov

-- | update position, if it is inside the bounds
updatePosP ::  Move -> Position -> Position
updatePosP  (Move xmov ymov) (Position xpos ypos) = if validPosition then Position newX newY else Position xpos ypos
  where validPosition :: Bool
        validPosition = not (outOfBounds (Position newX newY) playerHitBox)
        newX          = xpos + xmov
        newY          = ypos + ymov

outOfBounds :: Position -> HitBox -> Bool
outOfBounds (Position posx posy) (HitBox width height) =  not validPosition 
  where validPosition :: Bool
        validPosition =    posx + posx <  screenx - (width `quot` 2)
                        && posx + posx> - screenx  + (width `quot` 2)
                        && posy + posy <  screeny - height
                        && posy + posy > -screeny + height
class Ship k where 
  isHit :: k -> Bullet -> Maybe Float
  getDamage :: Float -> k -> k

instance Ship Player where 
  isHit player@(Player {pos, hitbox, health}) (Bullet bulletpos (BulletType _ size dmg _) _)    | collision (hitbox, pos) (size, bulletpos) = Just dmg
                                                                                                | otherwise = Nothing
  getDamage dmg player@(Player { health}) | dmg <= 0 = player
                                          | otherwise = player {health = health - dmg, hitAnim = 1}
instance Ship Enemy where
  isHit enemy@(Enemy {epos, ehitbox, ehealth}) (Bullet bulletpos (BulletType _ size dmg _) _)    | collision (ehitbox, epos) (size, bulletpos) = Just dmg
                                                                                             | otherwise = Nothing
  getDamage dmg enemy@(Enemy { ehealth}) | dmg <= 0 = enemy
                                         | otherwise =  enemy {ehealth = ehealth - dmg, eHitAnim = 1}
                                         

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

enemyColl :: Player -> Enemy -> (Float, Enemy)

enemyColl player@(Player {pos, hitbox, health}) enemy@(Enemy {epos, ehitbox, ehealth})    | collision (hitbox, pos) (ehitbox, epos) = (collDamage, enemy {ehealth= ehealth-collDamage, eHitAnim=1})
                                                                                          | otherwise = (0, enemy)

enemyKill :: Enemy -> Maybe Enemy
enemyKill enemy@Enemy{ehealth, killAnim}                            | ehealth > 0 = Just enemy
                                                                    | killAnim == 0 = Just enemy{killAnim = 1}
                                                                    | killAnim < 0 = Nothing
                                                                    | otherwise = Just enemy
enemyScore :: Enemy -> Int
enemyScore enemy@Enemy{ehealth, killpoints, killAnim}               | ehealth > 0 = 0
                                                                    | killAnim == 1 = killpoints
                                                                    | otherwise = 0

enemyShoot ::  Float -> Enemy -> (Maybe Bullet,Enemy)
enemyShoot secs enem@Enemy{eBullet, eLastFire, epos = (Position xp yp), ehitbox= (HitBox x y)} | enemyCanShoot enem = (Just (Bullet (Position xp (yp- (fromIntegral y) `div` 2 )) eBullet 0) , enem{ eLastFire = 0}  )
                                                                                               | otherwise = (Nothing,  enem{ eLastFire = secs + eLastFire})

enemyCanShoot :: Enemy -> Bool                                            
enemyCanShoot Enemy{efireRate, eLastFire} | efireRate < eLastFire = True
                                          | otherwise = False
                                          
collision:: (HitBox, Position) -> (HitBox, Position) -> Bool
collision (HitBox w1 h1, Position x1 y1) (HitBox w2 h2, Position x2 y2) = ((x1 - b1) < (x2 + b2)) &&
                                                                          ((x1 + b1) > (x2 - b2)) &&
                                                                          ((y1 - v1) < (y2 + v2)) &&
                                                                          ((y1 + v1)  > (y2 - v2))
                                                                          

                                                                          where b1 = (w1 `quot` 2)
                                                                                b2 = (w2 `quot` 2)
                                                                                v1 = (h1 `quot` 2)
                                                                                v2 = (h2 `quot` 2)
                              
-- ********************* CONSTANTS ***************
initialState :: StdGen -> GameState
--Starting phase
initialState = GameState 
                         0 
                         MainMenu
                         (Player beginPos playerHitBox 0.5 standardBullet 0 100 0 0) 
                         (KeysPressed False False False False False) 
                         [] 
                         []
                         1
                         1
                         0
                         " "
                         "default"

selectEnemy :: Int -> Int -> Enemy
--                   position   hitbox  fireRate  bullettype      lastfire  health  model ai  speed killpoints  HitAnim 
selectEnemy d sel = [Enemy enemySpawn eHitBox (1/0)     standardEBullet 0         20       0     4   2    (1000*d)        0     0
                    ,Enemy enemySpawn eHitBox 1         standardEBullet 0         5       1     1   2     (20*d)          0     0
                    ,Enemy enemySpawn eHitBox 1         standardEBullet 0         5       2     2   2     (30*d)          0     0
                    ,Enemy enemySpawn eHitBox 1         standardEBullet 0         5       3     3   2     (100*d )        0     0
                    ,Enemy enemySpawn eHitBox 1         standardEBullet 0         5       3     4   2     (1*d)           0     0]!!sel

standardBullet :: BulletType
standardBullet = (BulletType bulletSpeed bulletHitBox bulletDamage (color red (circleSolid 5)) )

standardEBullet :: BulletType
standardEBullet = (BulletType (Move 0 (-10)) bulletHitBox bulletDamage (color red (circleSolid 5)) )
--moventSpeed of the player
movementSpeed :: Int
movementSpeed = 5

bulletSpeed :: Move
bulletSpeed = Move 0 10

bulletHitBox :: HitBox
bulletHitBox = HitBox 10 10 

bulletDamage :: Float
bulletDamage = 10

screenx :: Int 
screenx = 608

screeny :: Int
screeny = 1080

playerHitBox :: HitBox
playerHitBox = HitBox 40 40

eHitBox :: HitBox
eHitBox = HitBox 100 100

beginPos :: Position
beginPos = Position 0 0

enemySpawn :: Position
enemySpawn = Position 0 560

collDamage :: Float
collDamage = 10

informationBar :: Int
informationBar = 200

informationScaler :: Float
informationScaler =  (fromIntegral screenx) / ( ( fromIntegral screenx) + ((fromIntegral informationBar) / 2))
