-- | This module contains the data types
--   which represent the state of the game
{-# language NamedFieldPuns #-}
module Model where

import Graphics.Gloss
import System.Random

-- ********************* DATA TYPES ***************
data GameState = GameState {
                   elapsedTime :: Float
                 , player :: Player
                 , keys :: KeysPressed
                 , paused :: Bool
                 , lost :: Bool
                 , bullets :: [Bullet]
                 , enemies :: [Enemy]
                 , level :: Int
                 , score :: Int
                 , randomGen :: StdGen}

data Bullet = Bullet { position:: Position, kind :: BulletType}

data BulletType =  BulletType { speed:: Move, size :: HitBox, damage :: Float}

data HitBox = HitBox { width :: Int, height :: Int}   

data Player = Player { pos :: Position, hitbox :: HitBox, fireRate :: Float, bullet :: BulletType, lastFire :: Float, health :: Float }

data Enemy = Enemy { epos :: Position, ehitbox :: HitBox, efireRate :: Float, eBullet :: BulletType, eLastFire :: Float, ehealth :: Float , epic :: Picture, eai :: Int}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool, space :: Bool }


-- ********************* FUNCTIONS ***************

randomSpawn :: Enemy -> Int
randomSpawn x = undefined

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
        validPosition =    posx + posx <  screenx - width
                        && posx + posx> - screenx  + width
                        && posy + posy <  screeny - height
                        && posy + posy > -screeny + height
class Ship k where 
  isHit :: k -> Bullet -> Maybe Float
  getDamage :: Float -> k -> k

instance Ship Player where 
  isHit player@(Player {pos, hitbox, health}) (Bullet bulletpos (BulletType _ size dmg))    | collision (hitbox, pos) (size, bulletpos) = Just dmg
                                                                                            | otherwise = Nothing
  getDamage dmg player@(Player { health}) = player {health = health - dmg}
instance Ship Enemy where
  isHit enemy@(Enemy {epos, ehitbox, ehealth}) (Bullet bulletpos (BulletType _ size dmg))    | collision (ehitbox, epos) (size, bulletpos) = Just dmg
                                                                                             | otherwise = Nothing
  getDamage dmg enemy@(Enemy { ehealth}) = enemy {ehealth = ehealth - dmg}
enemyColl :: Player -> Enemy -> (Float, Enemy)

enemyColl player@(Player {pos, hitbox, health}) enemy@(Enemy {epos, ehitbox, ehealth}) | collision (hitbox, pos) (ehitbox, epos) = (collDamage, enemy {ehealth= ehealth-collDamage})
                                                                                          | otherwise = (0, enemy)


collision:: (HitBox, Position) -> (HitBox, Position) -> Bool
collision (HitBox w1 h1, Position x1 y1) (HitBox w2 h2, Position x2 y2) = (x1  < x2 + w2) &&
                                                                          (x1 + w1 > x2) &&
                                                                          (y1 < y2 + h2) &&
                                                                          (y1 + h1 > y2)
-- ********************* CONSTANTS ***************
initialState :: StdGen -> GameState
--Starting phase
initialState = GameState 0 
                         (Player beginPos playerHitBox 1 standardBullet 0 100) 
                         (KeysPressed False False False False False) 
                         False 
                         False 
                         [] 
                         []
                         1
                         0
standardBullet :: BulletType
standardBullet = (BulletType bulletSpeed bulletHitBox bulletDamage)

--moventSpeed of the player
movementSpeed :: Int
movementSpeed = 10

bulletSpeed :: Move
bulletSpeed = Move 0 5

bulletHitBox :: HitBox
bulletHitBox = HitBox 10 10 

bulletDamage :: Float
bulletDamage = 10

screenx :: Int 
screenx = 608

screeny :: Int
screeny = 1080

playerHitBox :: HitBox
playerHitBox = HitBox 20 20

eHitBox :: HitBox
eHitBox = HitBox 50 50

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
