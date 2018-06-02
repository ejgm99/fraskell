module Display (display) where

import Graphics.UI.GLUT
import Basic

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    mapM squareRend (square' 0 0 0.4 square)
    print "flushing"
    flush
    