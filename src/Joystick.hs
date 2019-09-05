module Joystick(handleKeys) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import PongGameState

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'w') Down _ _) game = 
    game { dp1 = 1 }

handleKeys (EventKey (Char 's') Down _ _) game =
    game { dp1 = -1 }

handleKeys (EventKey (Char 'o') Down _ _) game =
    game { dp2 = 1 }

handleKeys (EventKey (Char 'l') Down _ _) game =
    game { dp2 = -1 }


handleKeys (EventKey (Char 'w') Up _ _) game = 
    game { dp1 = 0 }

handleKeys (EventKey (Char 's') Up _ _) game =
    game { dp1 = 0 }

handleKeys (EventKey (Char 'o') Up _ _) game =
    game { dp2 = 0 }

handleKeys (EventKey (Char 'l') Up _ _) game =
    game { dp2 = 0 }
  

-- Do nothing for all other events.
handleKeys _ game = game