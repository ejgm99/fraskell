module Shape where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data P =  P { x :: GLfloat, y :: GLfloat} deriving Show

class Shape a where
    points :: a -> [(GLfloat, GLfloat)]
    sideLength :: a -> GLfloat

data Regular = Square { point1 :: P, point2 :: P, dir :: GLfloat } deriving Show

instance Shape Regular where
    points (Square (P x1 y1) (P x2 y2) dir) = squarePoints (Square (P x1 y1) (P x2 y2) dir)
    sideLength (Square (P x1 y1) (P x2 y2) _) = distance (P x1 y1) (P x2 y2) 

showPoints ::(Shape a) => a -> [(GLfloat, GLfloat)]
showPoints a = points a

distance :: P -> P -> GLfloat
distance (P x1 y1) (P x2 y2) = ( (x**2) + (y**2) ** (0.5))
    where x = abs (x2 - x1)
          y = abs (y2 - y1)

squarePoints :: Regular -> [(GLfloat, GLfloat)]
squarePoints (Square (P x1 y1) (P x2 y2) dir) 
        | colinear (P x1 y1) (P x2 y2) = [ (x1 , y1) , (x2,y2) , (x1+(y*dir), y1+(x*dir)) , (x2+(y*dir), y2+(x*dir))]
        | otherwise                    = [ (x1 , y1) , (x2,y2) , (x1-dx, y1-dy) , (x2-dx, y2-dy)]
            where x = abs (x2-x1)
                  y = abs (y2-y1)
	          theta = pi - ((pi/2) + atan (abs (x/y) ) )
                  r = ((x**2)+(y**2)) ** (0.5)
	          dx = (acos( theta ))* r * dir
	          dy = (asin( theta ))* r * dir

colinear :: P -> P -> Bool
colinear (P x1 y1) (P x2 y2)
    | (x2 - x1) == 0 || (y2 - y1) == 0 = True
    | otherwise = False

