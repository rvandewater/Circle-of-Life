-- | This module contains the data types
--   which represent the state of the game
module Model where
         
data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool }
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

screenx :: Int 
screenx = 720
screeny :: Int
screeny = 1280

updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = if validPosition then Position newX newY else Position xpos ypos
  where validPosition :: Bool
        validPosition =    newX + newX <  screenx
                        && newX + newX > -screenx
                        && newY + newY <  screeny
                        && newY + newY > -screeny
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