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
viewPure gstate@(GameState s (Position xpos ypos) _ _ bul) = pictures [(translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 circleSize))), bulletVisual gstate]

bulletVisual :: GameState -> Picture 
bulletVisual gstate@(GameState {bullets}) = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) _ _ _ ) = translate (fromIntegral xpos) (fromIntegral ypos) bulletPic

bulletPic :: Picture
bulletPic = color red (thickCircle 2 5)


