module Points where

import Graphics.Rendering.OpenGL


circularPoints :: Int -> [(GLfloat, GLfloat, GLfloat)]
circularPoints n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
		where n' = fromIntegral n

points :: Int -> [(GLfloat, GLfloat, GLfloat)]
points n = [(k*1.0,k*1.0,k*1.0) | k<- [1..n'] ]
        where n' = fromIntegral n
