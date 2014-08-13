module Render.Init
( withWindow
, clearWindow
)
where

--------------------------------------------------------------------------------

import           System.Environment

import           Data.List                 (intercalate)
import           Data.Maybe
--import Control.Monoid
import           Control.Monad
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Render.Core.Error
-------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    --GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3

    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

clearWindow :: GLFW.Window -> IO ()
clearWindow window = do
	GL.clearColor $= GL.Color4 1 1 1 1
	logGL "clearWindow: clearColor"
	GL.clear [GL.ColorBuffer]
	logGL "clearWindow: clear"
	(width, height) <- GLFW.getFramebufferSize window
	GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
	logGL "clearWindow: viewport"

