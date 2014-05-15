module Main where

import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW as GLFW
import Data.StateVar
import Data.IORef
import System.Exit
import Data.Maybe
import Foreign.Marshal
import Foreign
import Foreign.C
import Control.Applicative
import Control.Monad.Loops
import Control.Monad
import Control.Concurrent

import Shader

foreign import ccall "GL/glew.h glewInit" glewInit :: IO CInt

-- Initializate all
initAll :: IO GLFW.Window
initAll = do
    window <- glfwInit
    --(w,h) <- GLFW.getFramebufferSize window
    --handleResize window w h
    callbacksInit window
    return window

-- When the close button is pressed
shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

--Called when the window is resized
handleResize :: GLFW.WindowSizeCallback
handleResize _ width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height) -- Screen limit
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  glOrtho 0 (toEnum width) 0 (toEnum height) (-1) 1
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

-- Defines all the W callbacks
callbacksInit :: GLFW.Window -> IO ()
callbacksInit window = do
    GLFW.setWindowSizeCallback window  (Just handleResize) -- Window size callback
    GLFW.setWindowCloseCallback window (Just shutdown ) -- Window close callbac

-- Initializate GLFW
glfwInit :: IO GLFW.Window
glfwInit = do
  GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 2
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
  -- Open Window
  maybeWindow <- GLFW.createWindow 1024 -- width
                                   768 -- height
                                   "Tutorial 2" -- Window name
                                   Nothing -- Monitor (used in fullscreen)
                                   Nothing -- Window for shared resources
  case maybeWindow of
    Nothing -> do
             putStrLn "Error on opening GLFW window"
             GLFW.terminate
             exitFailure
    Just window -> do
        glewInit
        GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
        GLFW.makeContextCurrent (Just window) -- Tell GLFW to use this window
        return window

main :: IO ()
main = do
    window <- initAll
    glClearColor 0.0 0.0 0.4 1.0 -- Clear color to dark blue
    -- Shader stuff
    alloca $ \vertexArrayIDPtr -> do
    glGenVertexArrays 1 vertexArrayIDPtr
    _ <- glBindVertexArray <$> peek vertexArrayIDPtr

    programID <- loadProgram "SimpleVertexShader.vertexshader" "SimpleFragmentShader.fragmentshader"

    vertexPosition_modelspaceID <-
      withCString "vertexPosition_modelspace" $
      (fromIntegral <$>) . glGetAttribLocation programID
    if (vertexPosition_modelspaceID < 0)
      then putStrLn "vertexPosition_modelspaceID could not be found!"
      else putStrLn "vertexPosition_modelspaceID loaded!"

    let g_vertex_buffer_data = [-1, -1, 0,
                                 1, -1, 0,
                                 0,  1, 0] :: [GLfloat]

    vertexBufferName <-
      alloca $ \vertexBufferPtr -> do
        glGenBuffers 1 vertexBufferPtr
        peek vertexBufferPtr
    glBindBuffer gl_ARRAY_BUFFER vertexBufferName
    withArrayLen g_vertex_buffer_data $ \len ptr -> do
      let size = fromIntegral $ len * sizeOf (head g_vertex_buffer_data)
      glBufferData gl_ARRAY_BUFFER size (ptr :: Ptr GLfloat) gl_STATIC_DRAW

    --Drawing
    GLFW.swapInterval 1
    ( do
        t0 <- fromJust <$> GLFW.getTime
        --putStrLn "== loop begin =="
        glClear gl_COLOR_BUFFER_BIT
        glUseProgram programID
        glEnableVertexAttribArray vertexPosition_modelspaceID
        glBindBuffer gl_ARRAY_BUFFER vertexBufferName
        glVertexAttribPointer vertexPosition_modelspaceID 3 gl_FLOAT (fromBool False) 0 nullPtr
        --putStrLn "Okj"
        glDrawArrays gl_TRIANGLES 0 3
        --putStrLn "lk"
        glDisableVertexAttribArray vertexPosition_modelspaceID
        GLFW.swapBuffers window
        GLFW.pollEvents
        t1 <- fromJust <$> GLFW.getTime
        let deltaT = realToFrac $ t1 - t0 :: Double
        threadDelay $ if deltaT > 1 / 60
            then 0
            else round $ 1000000 * ((1 / 60) - deltaT)
        t2 <- fromJust <$> GLFW.getTime
        putStrLn $ show ((1 :: Double) / realToFrac (t1 - t0))
                ++ " -> "
                ++ show ((1 :: Double) / realToFrac (t2 - t0))
        --putStrLn "== loop end =="
     ) `untilM_` ((||) <$> ((==) GLFW.KeyState'Pressed <$> GLFW.getKey window GLFW.Key'Escape)
                       <*> GLFW.windowShouldClose window)
    GLFW.terminate
    alloca $ \ptr -> do
      glDeleteBuffers 1 ptr
    glDeleteVertexArrays 1 vertexArrayIDPtr
    glDeleteProgram programID
