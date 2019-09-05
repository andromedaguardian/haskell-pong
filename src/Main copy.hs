module Main(main) where

import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width  = 800
height = 600
offset = 50
paddleWidth, paddleHeight, paddleOffset :: Float
paddleWidth = 20
paddleHeight = 80
paddleOffset = 20
-- | paddles' move speed
playerVel :: Float
playerVel = 10


-- | A data structure to hold the state of the Pong game.
-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  , dp1   :: Float             -- ^ Player 1 direction
  , dp2   :: Float             -- ^ Player 2 direction
  } deriving Show


-- | Draw a pong game state (convert it to a picture).
-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball,
            mkPaddle rose (fromIntegral (-width)/2 + paddleOffset) $ player1 game,
            mkPaddle orange (fromIntegral width/2 - paddleOffset) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor $ rectangleSolid paddleWidth paddleHeight
      ]

    paddleColor = light (light blue)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
    -> PongGame -- ^ The initial game state
    -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
    where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds


-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
ballBounce :: PongGame -> PongGame
ballBounce game = game { ballVel = (vx', vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             (-vy) * 1.005719
           else
            -- Do nothing. Return the old velocity.
            vy

    vx' = if paddleCollision game radius
          then
            -- reverse the direction and make the game harder each time it hits a paddle
            -vx * 1.03
          else
            vx

type Radius = Float 
type Position = (Float, Float)
-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >=  fromIntegral height / 2

-- | Check if the ball hits a paddle(left or right). this function implements
-- a simple AABB collision
paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game r
    -- checking if the ball overlap the left paddle
    | (x1 + w) > bx && (x1 - w) < bx &&
      (y1 + h) > by && (y1 - h) < by =
        True
    | (x2 - w) < bx && (x2 + w) > bx &&
      (y2 + h) > by && (y2 - h) < by =
          True
    | otherwise = False
    where
        x1 = (fromIntegral (-width)/2 + paddleOffset)
        x2 = (fromIntegral width/2 - paddleOffset)
        y1 = (player1 game)
        y2 = (player2 game)
        (bx , by) = (ballLoc game)
        w = paddleWidth / 2
        h = paddleHeight / 2

-- | Update all players position using its current Y-direction.
movePaddle :: PongGame -> PongGame
movePaddle game = game { player1 = p1, player2 = p2}
    where
        p1 = (player1 game) + playerVel * (dp1 game)
        p2 = (player2 game) + playerVel * (dp2 game)

-- | Detect if a paddle hit the top/bottom edge. If hit
-- it stops to move at that direction
paddleConstraints :: PongGame -> PongGame
paddleConstraints game = game {player1 = p1, player2 = p2}
    where
        p1
            | paddleHitTop (player1 game) =
                fromIntegral height / 2 - paddleHeight / 2
            | paddleHitBottom (player1 game) =
                fromIntegral (-height) / 2 + paddleHeight / 2
            | otherwise = (player1 game)
        p2
            | paddleHitTop (player2 game) =
                fromIntegral height / 2 - paddleHeight / 2
            | paddleHitBottom (player2 game) =
                fromIntegral (-height) / 2 + paddleHeight / 2
            | otherwise = (player2 game)


paddleHitTop :: Float -> Bool
paddleHitTop y
    | y > ((fromIntegral height / 2) - paddleHeight / 2) = True
    | otherwise = False

paddleHitBottom :: Float -> Bool
paddleHitBottom y
    | y < (fromIntegral (-(height)) / 2 + paddleHeight / 2) = True
    | otherwise = False


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


-- | Initialize the game with this game state.
-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (40, -100)
  , player1 = 40
  , player2 = -80
  , dp1 = 0
  , dp2 = 0
  }

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update  seconds game = 
    ballBounce  (paddleConstraints (movePaddle (moveBall seconds game)))
    