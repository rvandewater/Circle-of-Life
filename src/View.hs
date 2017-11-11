{-# language NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import Data.Fixed
import Data.List.Split
import Data.List

-- Generate display
view :: [Picture] -> GameState -> IO Picture
view bg gs@GameState{screen,score, scorelist, plrname, runTime} 
        | screen == MainMenu            = return (mainMenuVisual gs)
        | screen == GameOver            = return (pictures [translate (-350) 400 (color (colorFlow runTime) (fatText "Game Over")), color green (pictures 
                                                           [translate (-200) 0 (fatText "Score:")
                                                           ,scale 1.5 1.5 (translate (-50*(fromIntegral (length (show score)))) (-200)  (fatText (show score)))
                                                           ,scale 0.4 0.4 (translate (-500)                                     (-1000) (fatText "Press any key")) ])])                     
        | screen == PausedGame          = return (pictures [(translate (-200) 0 (color (colorFlow runTime) (fatText "Paused"))),(scale 0.4 0.4 (translate (-1000) (-850) (color green( fatText "Press r to go to Main Menu"))))])
        | screen == DifficultySelect    = return (scale 0.5 0.5 (color green (pictures [(translate (- ( fromIntegral screenx)+150) 0 (fatText "Select your difficulty"))
                                                                          ,translate 0 (-200) (fatText "1")
                                                                          ,translate 0 (-330) (fatText "2")
                                                                          ,translate 0 (-460) (fatText "3")])))
        | screen == LevelSelect         = return (scale 0.5 0.5 (color green (pictures [(translate (- ( fromIntegral screenx)+300) 0 (fatText "Select your level"))
                                                                          ,translate 0 (-200) (fatText "1")
                                                                          ,translate 0 (-330) (fatText "2")
                                                                          ,translate 0 (-460) (fatText "3") ])))
        | screen == HighScores          = return (highScoreParse scorelist)
        | screen == PlayGame            = return (gameView bg gs)
        | screen == (WriteScore False ) = return (color green (pictures [(translate (-200) 0 (fatText plrname))
                                                                        ,(scale 0.5 0.5 (translate (-600)   800  (fatText "Write your name")))
                                                                        ,(scale 0.5 0.5 (translate (-700) (-800) (fatText "Press Enter to finish")))]))
        | otherwise                     = return blank

gameView :: [Picture] -> GameState -> Picture
gameView bg gstate@GameState{score, screen}             = pictures [updateBg bg gstate, playerVisual gstate, bulletVisual gstate, enemyVisual gstate, informationVisual gstate, powerUpsVisual gstate  ]

fatText str = pictures [translate x y stdtext| x <-[-4..4], y<-[-4..4]]
        where stdtext = (Text str)
              
-- Updating background
updateBg :: [Picture] -> GameState -> Picture
updateBg bg gs@GameState{level} =  translate 0 (1080 - (mod' (200 * elapsedTime gs) 2160)) (bg!!(level-1))            --putting background to the beginning and looping depending on the elapsed time

mainMenuVisual :: GameState -> Picture
mainMenuVisual GameState{runTime} = (pictures[(scale 0.7 0.7(translate (-400) 600 (color green (fatText "Circle of life")))), 
                                scale 2 2 (translate (15) (225) (color (colorFlow runTime) (thickCircle 10 20))), (flash (scale 0.5 0.5 (translate (-700) (-200) (color green( fatText "Press Space to Start")))) runTime), 
                                (scale 0.4 0.4 (translate (-1010) (-850) (color green( fatText "Press Enter for Recent Scores")))), scale 2 2 (scrollBar (getModel (mod (ceiling runTime) 5)) runTime) ])
scrollBar :: Picture -> Float -> Picture
scrollBar k time = (translate ((mod'(time*100) (fromIntegral screenx))-  (fromIntegral screenx /2)) 100 k)

powerUpsVisual :: GameState -> Picture
powerUpsVisual GameState{powerups, elapsedTime} = pictures (map (powerUpVisual elapsedTime) powerups) 

powerUpVisual :: Float -> PowerUp -> Picture
powerUpVisual rt (Health _ (Position x y) _)  =  (translate (fromIntegral x)  (fromIntegral y) (pictures [ (color (colorFlow  rt) (rectangleSolid 35 30)),translate (-17) (-13) (flash  ((scale 0.2 0.2 (color black (fatText "HP")))) rt)]))
powerUpVisual rt (FireRate _ (Position x y) _)  = (translate (fromIntegral x)  (fromIntegral y) (pictures [(translate 17 10 (color (colorFlow  rt) (rectangleSolid 35 30))),(flash  ((scale 0.2 0.2 (color black (fatText "FR"))))) rt]))
powerUpVisual rt (Damage _ (Position x y) _)  = (translate (fromIntegral x)  (fromIntegral y) (pictures [(translate 17 10 (color (colorFlow  rt) (rectangleSolid 35 30))),(flash  ((scale 0.2 0.2 (color black (fatText "DM"))))) rt]))

flash :: Picture -> Float -> Picture
flash k time | (ceiling time `mod` 2) == 0 = blank
             | otherwise = k

colorFlow :: Float -> Color        
colorFlow time | ( mod' time 2)< 1 = (makeColor (1 - ( mod' time 1)) ( mod' time 1 ) (1 - ( mod' time 1)) 1)
               | otherwise = (makeColor ( mod' time 1 ) (1 - ( mod' time 1)) ( mod' time 1 ) 1)

-- Visualizing player
playerVisual :: GameState -> Picture 
playerVisual GameState{player = Player {pos = Position{xpos, ypos}, hitbox = HitBox{width},hitAnim}} | (hitAnim>0 ) = translate (fromIntegral xpos) (fromIntegral ypos) (color (makeColor hitAnim (1-hitAnim) 0 1) (circleSolid 20))
                                                                                                     | otherwise = translate (fromIntegral xpos) (fromIntegral ypos) (color green (circleSolid 20))

highScoreParse :: String -> Picture
highScoreParse scores = pictures [(translate 0 (400) (pictures (translated))), ( (scale 0.5 0.5) (translate (-500) (800) (color green( fatText "Recent Scores"))))]
                        where   translated = map finalpics (zip [0..] pics)
                                finalpics (y, pic) =  (translate (-300) (y*(-100)-100)) pic 
                                pics = map (scale 0.5 0.5) (map (color green) (columnbar: (map rower elements)))
                                columnbar = pictures[(translate (-100) 0 (fatText "Name")),  (translate (400) 0 (fatText "Score")), (translate (800) 0 (fatText "Lvl")), (translate (1000) 0 (fatText "Dif"))]
                                rower row= pictures[(translate (-100) 0 (row!!0)),  (translate (400) 0 (row!!1)), (translate (800) 0 (row!!2)), (translate (1000) 0 (row!!3))]
                                elements = (map.map) fatText(map (splitOn "%") scorelist)
                                scorelist =  (tail (reverse(splitOn "~" scores)))

-- Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual GameState{enemies} = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) _ _ _ _  ai _ _ eanim killanim)       | (killanim > 0) = translate (fromIntegral xpos) (fromIntegral ypos)(scale (killanim*(1/scoreAnim)) (killanim*(1/scoreAnim)) (getModel ai))
                                                                                        | (eanim > 0) &&((floor(eanim*10)) `mod` 2  == 0) = blank
                                                                                        | otherwise = translate (fromIntegral xpos) (fromIntegral ypos) ( (getModel ai))

getModel :: Int -> Picture
getModel m | m == 0             = color white (rectangleSolid 50 50)
           | m == 1             = rotate 45 (color blue (rectangleSolid 50 50))
           | m == 2             = pictures [color yellow (rectangleSolid 50 50), rotate 45 (color yellow (rectangleSolid 50 50))]
           | m == 3             = color red (triangle 50)
           | m == 4             = pictures [color cyan (translate 0 20 (rotate 180 (triangle 50))),color black (translate 0 (-20) (triangle 50))]
           | otherwise          = (color black (rectangleSolid 50 50))
                        where triangle s = polygon [ (0,0), (s / 2, s), ((-s) / 2,s), (0,0) ]
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
informationVisual gstate = pictures[(translate  0 ((fromIntegral screeny/2)) (color (makeColor 0.5 0.5 0.5 0.5) (rectangleSolid (fromIntegral screenx) 120))),
         (translate ( 20 -( fromIntegral screenx/2)) ((fromIntegral screeny/2)-50 ) (scale 0.4 0.4(color red (fatText (show (health(player gstate))++" HP"))))), 
     (translate ( (fromIntegral screenx/2) -300) ((fromIntegral screeny/2)-50) ( scale 0.4 0.4 (color red (fatText ("Score "++ show (score gstate))))))]





