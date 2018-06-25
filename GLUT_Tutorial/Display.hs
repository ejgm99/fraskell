module Display (display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Cube
import Points
 
display :: DisplayCallback
display = do
  clear [ColorBuffer]
  scale 0.5 0.5 (0.5::GLfloat)
  let x = 0.0
  let y = 0.0
  let z = 0.0
  preservingMatrix $ do
    color $ Color3 (x) (y+100) (z+100) 
    let size = 2
    let shapes = [(square size x y)]
    let x' = (x+(size/2.0))
    let y' = (y-(size/2.0))
    let shapes' = doTesselation mapShapes minimumSize shapes
    let shapes'' = doTesselation mapShapes minimumSize shapes'
    renderShapes shapes''
  flush
 
doTesselation :: ([Shape] -> [Shape]) -> ([Shape] -> GLfloat -> Bool) -> [Shape] -> [Shape]
doTesselation tessalation condition shapes --tesselation function generates new list of shapes 
  | conditionSatisfied = shapes
  | otherwise = shapes ++ doTesselation tessalation condition (tessalation shapes)
  where conditionSatisfied = (condition shapes 0.08)

mapShapes :: [Shape] -> [Shape]
mapShapes shapes
  | shapes == [] = shapes
  | otherwise = newShapes ++ (mapShapes (tail shapes))
  where newShapes = basicKochVariation (head shapes)


basicKochVariation :: Shape -> [Shape]
basicKochVariation shape =[square1,square2,square3, square4]
  where square1 = square (size'/2.0) (x+(size'/2.0)) (y-(size'/2.0))
        square2 = square (size'/2.0) (x-size')       (y)
        square3 = square (size'/2.0) (x)             (y+size'/2.0)
        square4 = square (size'/2.0) (x-(size'/2.0)) (y-size')
        size' = size shape
        x = xLoc shape
        y = yLoc shape


minimumSize :: [Shape] -> GLfloat -> Bool
minimumSize shapes min
  | size (last shapes) < min = True
  | otherwise = False

