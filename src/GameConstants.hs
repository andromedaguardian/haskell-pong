module GameConstants(
    width,
    height,
    offset,
    minBallVel,
    ballRadius,
    paddleWidth,
    paddleHeight,
    paddleOffset,
    minPaddleVel)where

width, height, offset :: Int
width  = 800                                        -- ^ window width
height = 600                                        -- ^ window height
offset = 50                                         -- ^ the position of the window

minBallVel, ballRadius :: Float
minBallVel = 25   
ballRadius = 10                                  -- ^ Ball velocity

paddleWidth, paddleHeight, paddleOffset, minPaddleVel :: Float
paddleWidth = 20                                    
paddleHeight = 80
paddleOffset = 20                                   -- ^ the distance between the paddle and the edge of the window
minPaddleVel = 10                                   -- ^he velocity which a paddle moves