-- | This module defines how to turn
--   the game state into a picture
{-# language NamedFieldPuns #-}
module View where
    

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState _ _ _ True _) = translate (-200) 0 (color green (Text "Paused"))
viewPure gstate = pictures [playerVisual gstate, bulletVisual gstate]

--generating player
playerVisual :: GameState -> Picture 
playerVisual gstate@(GameState s (Player (Position xpos ypos) (HitBox x y) ) _ _ bul) = translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 (fromIntegral x)))

--generating bullets
bulletVisual :: GameState -> Picture 
bulletVisual gstate@(GameState {bullets}) = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) _ _ _ ) = translate (fromIntegral xpos) (fromIntegral ypos) bulletPic

bulletPic :: Picture
bulletPic = color red (thickCircle 2 5)


