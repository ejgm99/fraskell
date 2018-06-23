module Display (display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Cube
import Points
 
display :: DisplayCallback
display = do
  clear [ColorBuffer]
  scale 0.7 0.7 (0.7::GLfloat)
  forM_ (points 12) $ \(x,y,z) ->
    preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      square 0.1
  flush