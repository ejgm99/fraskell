module Cube where
 
import Graphics.UI.GLUT 

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f [( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w), ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w), ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w), (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w), ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w), ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w)]

vertex2f :: (GLfloat, GLfloat) -> IO ()
vertex2f (x, y) = vertex $ Vertex3 x y 0

square :: GLfloat -> IO ()
square w = renderPrimitive LineLoop $ mapM_ vertex2f [( w, w), ( -w, w), ( -w, -w), ( w,-w)]

