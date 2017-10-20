-- | This module contains the data types
--   which represent the state of the game
module Model where
         
data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool }

screenx :: Int 
screenx = 720
screeny :: Int
screeny = 1280
circleSize :: Float
circleSize = 20

-- | update position, if it is inside the bounds
updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = if validPosition then Position newX newY else Position xpos ypos
  where validPosition :: Bool
        validPosition =    newX + newX <  screenx - round circleSize 
                        && newX + newX > -screenx + round circleSize 
                        && newY + newY <  screeny - round circleSize 
                        && newY + newY > -screeny + round circleSize 
        newX          = xpos + xmov
        newY          = ypos + ymov


data GameState = GameState {
                   elapsedTime :: Float
                 , playerPos :: Position
                 , keys :: KeysPressed
                 , paused :: Bool
                 }

initialState :: GameState
initialState = GameState 0 (Position 0 0) (KeysPressed False False False False) False


movementSpeed :: Int
movementSpeed = 10