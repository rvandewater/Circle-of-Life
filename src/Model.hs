-- | This module contains the data types
--   which represent the state of the game
module Model where
         
data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool }
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = Position (xpos+xmov) (ypos+ymov)

data GameState = GameState {
                   elapsedTime :: Float
                 , playerPos :: Position
                 , keys :: KeysPressed
                 , paused :: Bool
                 }

initialState :: GameState
initialState = GameState 0 (Position 0 0) (KeysPressed False False False False) False


movementSpeed :: Int
movementSpeed = 5