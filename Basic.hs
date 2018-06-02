module Basic where
import Graphics.UI.GLUT
import Shape



square :: GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat)] 
square x y s = [ (x,y), (x, y+s), (x+s, y+s), (x+s, y)] 


square'' :: (Shape a) => a -> (a -> [a]) -> [ [(GLfloat, GLfloat)] ]
square'' shape f
    | (sideLength shape) > 0.01 = []
    | otherwise = [points shape] ++ otherPoints
    where otherPoints = map square'' (f shape)
    

square' :: GLfloat -> GLfloat -> GLfloat -> (GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat)] ) -> [ [ (GLfloat, GLfloat) ] ]
square' x y s f
    | s < 0.01 = []
    | otherwise= [(f x y s)] ++ upper ++ left ++ lower ++ right
    where upper = square'  x        (y-(s/2)) (s/2) f
    	  left  = square' (x-(s/2)) (y+(s/2)) (s/2) f
	  lower = square' (x+(s/2)) (y+s)     (s/2) f
	  right = square' (x+s)     y         (s/2) f

squareListRend :: [[(GLfloat,GLfloat)]] -> [IO ()]
squareListRend [] = []
squareListRend x = squareRend (head x)  : squareListRend (tail x)
    
squareRend :: [(GLfloat,GLfloat)] -> IO ()
squareRend x = do
    renderPrimitive LineLoop $ do
        mapM_ (\(x, y) -> vertex $ Vertex2 x y) x