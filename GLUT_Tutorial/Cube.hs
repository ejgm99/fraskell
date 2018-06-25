module Cube where
 
import Graphics.UI.GLUT 

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

data Shape =Square {size :: GLfloat, location :: [GLfloat]} deriving (Show, Eq)

pointsList :: Shape -> [(GLfloat, GLfloat)]
pointsList square = [( x, y), ( x-size', y), ( x-size', y-size'), ( x,y-size')]
  where x = xLoc square
        y = yLoc square
        size' = size square

xLoc :: Shape -> GLfloat
xLoc shape = head (location shape)

yLoc :: Shape -> GLfloat
yLoc shape = last (location shape)

vertex2f :: (GLfloat, GLfloat) -> IO ()
vertex2f (x, y) = vertex $ Vertex3 x y 0

square :: GLfloat -> GLfloat -> GLfloat -> Shape
square w x y =Square w [x , y]

renderLines :: Shape -> IO()
renderLines shape = renderPrimitive LineLoop $ mapM_ vertex2f (pointsList shape)

renderShape :: Shape -> IO()
renderShape shape = do
    print (shape)
    --translate' (location shape)
    renderLines shape

renderShapes :: [Shape] -> IO()
renderShapes shapes = mapM_ renderShape shapes

translate' location = do translate $ Vector3 (head location) (last location) 0
