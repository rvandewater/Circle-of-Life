-- | This module defines how to turn
--   the game state into a picture
{-# language NamedFieldPuns #-}
module View where
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Gloss
import Model

--Generate display
view :: GameState -> IO Picture
view gs = return (pictures [backGround gs, viewPure gs])

--Generating background
backGround :: GameState -> Picture
{-# NOINLINE backGround #-} 
backGround gs = unsafePerformIO $ loadBMP "bg.bmp"

viewPure :: GameState -> Picture
--Pause screen
viewPure (GameState _ _ _ True _ _ _ _) = translate (-200) 0 (color green (Text "Paused"))

--Default game screen
viewPure gstate = pictures [playerVisual gstate, bulletVisual gstate, enemyVisual gstate]

--Visualizing player
playerVisual :: GameState -> Picture 
playerVisual gstate@(GameState s (Player (Position xpos ypos) (HitBox x y) _ _ _ ) _ _ bul _ _ _) = translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 (fromIntegral x)))

--Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual gstate@(GameState _ _ _ _ _ _ enemies _) = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) _ _ _) = translate (fromIntegral xpos) (fromIntegral ypos) (color red (rectangleSolid 50 50))

--Visualizing bullets
bulletVisual :: GameState -> Picture 
bulletVisual gstate@(GameState {bullets}) = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) _ ) = translate (fromIntegral xpos) (fromIntegral ypos) bulletPic

bulletPic :: Picture
bulletPic = color red (thickCircle 2 5)


