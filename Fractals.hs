import Graphics.UI.GLUT
import Bindings

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Fractals"
    displayCallback $= display
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just keyboardMouse
    mainLoop
    