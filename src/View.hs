-- | This module defines how to turn
--   the game state into a picture
{-# language NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import Data.Fixed
import Data.List.Split
import Data.List
--Generate display
view :: Picture -> GameState -> IO Picture
view bg gs@GameState{screen,score, scorelist, plrname}  | screen == MainMenu = return (pictures[(scale 0.5 0.5(translate (-fromIntegral screenx) 0 (color green (Text "Circle of life")))), (translate 0 (-200) (color green (thickCircle 10 20)))])
                                                        | screen == GameOver      = return (pictures [translate (-350) 400 (color green (Text "Game Over")),  translate (-200) 0 (color green (Text "Score:")), scale 1.5 1.5(translate (-100) (-200) (color green (Text (show score))))])
                                                        | screen == PausedGame  = return (translate (-200) 0 (color green (Text "Paused")))
                                                        | screen == DifficultySelect  = return (scale 0.5 0.5 (pictures [ (translate (- ( fromIntegral screenx)) 0 (color green (Text "Select your difficulty"))), translate 0 (-200) (color green (Text "1")),
                                                                translate 0 (-310) (color green (Text "2")), translate 0 (-420) (color green (Text "3")) ]))
                                                        | screen == LevelSelect  = return (scale 0.5 0.5 (pictures [ (translate (- ( fromIntegral screenx)) 0 (color green (Text "Select your level"))), translate 0 (-200) (color green (Text "1")),
                                                                translate 0 (-310) (color green (Text "2")), translate 0 (-420) (color green (Text "3")) ]))
                                                        | screen == HighScores = return (highScoreParse scorelist)
                                                        | screen == PlayGame = return (viewPure bg gs)
                                                        | screen == (WriteScore False ) = return (pictures [(translate (-200) 0 (color green (Text plrname))) , ( scale 0.5 0.5 (translate (-600) (800) (color green( text "Write your name")))),
                                                                 (scale 0.5 0.5 (translate (-700) (-800) (color green( text "Press Enter to finish")))) ])
                                                        | otherwise = return blank

viewPure :: Picture -> GameState -> Picture
viewPure bg gstate@GameState{score, screen}             = pictures [updateBg bg gstate, playerVisual gstate, bulletVisual gstate, enemyVisual gstate, informationVisual gstate    ]
--Updating background
updateBg :: Picture -> GameState -> Picture
updateBg bg gs = scale informationScaler 1 (translate 0 (-(mod' (100 * (elapsedTime gs)) 1440) + 720) bg)

--Visualizing player
playerVisual :: GameState -> Picture 
playerVisual GameState{player = Player {pos = Position{xpos, ypos}, hitbox = HitBox{width},hitAnim}} | (hitAnim>0 ) = translate (fromIntegral xpos) (fromIntegral ypos) (color (makeColor hitAnim (1-hitAnim) 0 1) (circleSolid 20))
                                                                                                     | otherwise = translate (fromIntegral xpos) (fromIntegral ypos) (color green (circleSolid 20))
highScoreParse :: String -> Picture
highScoreParse scores = pictures [(translate 0 (400) (pictures (translated))), ( (scale 0.5 0.5) (translate (-300) (800) (color green( text "High scores"))))]
                        where   translated = map finalpics (zip [0..] pics)
                                finalpics (y, pic) =  (translate (-300) (y*(-100)-100)) pic 
                                pics = map (scale 0.5 0.5) (map (color green) (map Text scorelist))
                                scorelist = splitOn "~" scores
                                --(sort(map (\x -> read x:: Int) scorelist))
--Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual GameState{enemies} = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) _ _ _ _  ai _ _ eanim killanim)   | (killanim > 0) = translate (fromIntegral xpos) (fromIntegral ypos)(scale killanim killanim (getModel ai))
                                                                                         | (eanim > 0) &&((floor(eanim*10)) `mod` 2  == 0) = blank
                                                                                         | otherwise = translate (fromIntegral xpos) (fromIntegral ypos) ( (getModel ai))

getModel :: Int -> Picture
getModel m | m == 0             = (color blue     (rectangleSolid 50 50))
           | m == 1             = (color white    (rectangleSolid 50 50))
           | m == 2             = (color yellow   (rectangleSolid 50 50))
           | m == 3             = (color red      (rectangleSolid 50 50))
           | m == 4             = (color black    (rectangleSolid 50 50))
           | otherwise          = (color black    (rectangleSolid 50 50))

--Visualizing bullets
bulletVisual :: GameState -> Picture 
bulletVisual GameState {bullets} = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) (BulletType {bulletpic, size = (HitBox x y), speed}) secs) = pictures [ pictures(map (speedLines secs speed) lines), (translate (fromIntegral xpos) (fromIntegral ypos) (bulletpic))]
                where linespaces = map (+ xpos)  (filter even [-(quot x 2).. (quot x 2)])
                      ymap x = (x, ypos) 
                      lines = map ymap linespaces

speedLines :: Float -> Move -> (Int, Int) -> Picture
speedLines secs (Move mx my) (xpos, ypos) = (color white (line [ (fromIntegral xpos, fromIntegral ypos),
        ((fromIntegral xpos), (fromIntegral ypos - secs* (fromIntegral my)))]))
        

informationVisual ::  GameState -> Picture
informationVisual gstate = pictures[ (translate (-  (((fromIntegral informationBar)/2) + fromIntegral screenx/2)) 0 (rotate 90(scale 0.5 0.5(color red (text (show (health(player gstate)))))))), 
     (translate (fromIntegral screenx/2) 0 (rotate 90( scale 0.5 0.5 (color red (text (show (score gstate)))))))]





