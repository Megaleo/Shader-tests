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
import Data.Vec hiding (head)

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

-- Checks to see if the shader's variable from the
-- shader was actually localizated. (Prints out to the screen
-- and returns again the varID)
checkVar :: String -> GLint -> IO GLint
checkVar varName varID = do
  if varID /= 0
    then putStrLn $ varName ++ " could not be localizated!"
    else putStrLn $ varName ++ " localizated!"
  return varID

localizateAttrib :: String -> GLuint -> IO GLint
localizateAttrib attrib programID =
  withCString attrib $ glGetAttribLocation programID

-- List of all variables attributes
attribList :: [String]
attribList = [ "vertexPosition_modelspace"
             , "vertexColor"]

localizateAllAttrib :: GLuint -> IO [GLuint]
localizateAllAttrib programID = do
  mapM (\var -> localizateAttrib var programID
            >>= checkVar var
            >>= return . fromIntegral) attribList

localizateUniform :: String -> GLuint -> IO GLint
localizateUniform uniform programID =
  withCString uniform $ glGetUniformLocation programID

-- List of all unifrom attributes
uniformList :: [String]
uniformList = [ "MVP" ]

localizateAllUniform :: GLuint -> IO [GLint]
localizateAllUniform programID = do
  mapM (\var -> localizateUniform var programID
            >>= checkVar var) uniformList

localizateVarying :: String -> GLuint -> IO GLint
localizateVarying varying programID =
  withCString varying $ glGetVaryingLocation programID

-- List of all unifrom attributes
varyingList :: [String]
varyingList = [ ]

localizateAllVarying :: GLuint -> IO [GLint]
localizateAllVarying programID = do
  mapM (\var -> localizateVarying var programID
            >>= checkVar var) varyingList

-- Helper function for 3D vectors
vec3 x y z = x :. y :. z :. ()

-- Model - View - Projection matrix
mvpMatrix :: Mat44 GLfloat
mvpMatrix = projection `multmm` view `multmm` model
  where projection = perspective 0.1 100 (pi/4) (4/3)
        view       = lookAt (vec3 0 1 0) (vec3 4 3 3) (vec3 0 0 0)
        model      = identity

-- Data.Vec rotationLookAt does not work exactly like I want,
-- temporary recoding
lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt up eye target = orientationM `multmm` translationM
  where
    zAxis = normalize (eye - target)
    xAxis = normalize $ up `cross` zAxis
    yAxis = zAxis `cross` xAxis
    orientationM = (homVec xAxis) :.
                  (homVec yAxis) :.
                  (homVec zAxis) :.
                  (0:.0 :.0:.1:.()) :. ()
    translationM = translation (-eye)

-- Fill a new buffer with data
fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer list = do
  bufId <- withNewPtr (glGenBuffers 1)
  glBindBuffer gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length *
                                  sizeOf (undefined :: GLfloat)))
                 (ptr :: Ptr GLfloat) gl_STATIC_DRAW
  return bufId

bindBufferToAttrib :: GLuint -> GLuint -> IO ()
bindBufferToAttrib bufId attribLoc = do
  glEnableVertexAttribArray attribLoc
  glBindBuffer gl_ARRAY_BUFFER bufId
  glVertexAttribPointer attribLoc  -- attribute location in the shader
                        3  -- 3 components per vertex
                        gl_FLOAT  -- coordinates type
                        (fromBool False)  -- normalize?
                        0  -- stride
                        nullPtr  -- vertex buffer offset

main :: IO ()
main = do
    window <- initAll
    glClearColor 0.0 0.0 0.4 1.0 -- Clear color to dark blue
    -- Shader stuff
    alloca $ \vertexArrayIDPtr -> do
    glGenVertexArrays 1 vertexArrayIDPtr
    _ <- glBindVertexArray <$> peek vertexArrayIDPtr

    programID <- loadProgram "SimpleVertexShader.vertexshader"
                             "SimpleFragmentShader.fragmentshader"

    [vertexPosition_modelspaceID, vertexColor] <- localizateAllAttrib programID
    [mvpID] <- localizateAllUniform programID

    let vertexBufferData = [-1.0,-1.0,-1.0, -- triangle 1 : begin
                            -1.0,-1.0, 1.0,
                            -1.0, 1.0, 1.0, -- triangle 1 : end
                            1.0, 1.0,-1.0, -- triangle 2 : begin
                            -1.0,-1.0,-1.0,
                            -1.0, 1.0,-1.0, -- triangle 2 : end
                            1.0,-1.0, 1.0,
                            -1.0,-1.0,-1.0,
                            1.0,-1.0,-1.0,
                            1.0, 1.0,-1.0,
                            1.0,-1.0,-1.0,
                            -1.0,-1.0,-1.0,
                            -1.0,-1.0,-1.0,
                            -1.0, 1.0, 1.0,
                            -1.0, 1.0,-1.0,
                            1.0,-1.0, 1.0,
                            -1.0,-1.0, 1.0,
                            -1.0,-1.0,-1.0,
                            -1.0, 1.0, 1.0,
                            -1.0,-1.0, 1.0,
                            1.0,-1.0, 1.0,
                            1.0, 1.0, 1.0,
                            1.0,-1.0,-1.0,
                            1.0, 1.0,-1.0,
                            1.0,-1.0,-1.0,
                            1.0, 1.0, 1.0,
                            1.0,-1.0, 1.0,
                            1.0, 1.0, 1.0,
                            1.0, 1.0,-1.0,
                            -1.0, 1.0,-1.0,
                            1.0, 1.0, 1.0,
                            -1.0, 1.0,-1.0,
                            -1.0, 1.0, 1.0,
                            1.0, 1.0, 1.0,
                            -1.0, 1.0, 1.0,
                            1.0,-1.0, 1.0]
        colorBufferData  = [0.583,  0.771,  0.014,
                            0.609,  0.115,  0.436,
                            0.327,  0.483,  0.844,
                            0.822,  0.569,  0.201,
                            0.435,  0.602,  0.223,
                            0.310,  0.747,  0.185,
                            0.597,  0.770,  0.761,
                            0.559,  0.436,  0.730,
                            0.359,  0.583,  0.152,
                            0.483,  0.596,  0.789,
                            0.559,  0.861,  0.639,
                            0.195,  0.548,  0.859,
                            0.014,  0.184,  0.576,
                            0.771,  0.328,  0.970,
                            0.406,  0.615,  0.116,
                            0.676,  0.977,  0.133,
                            0.971,  0.572,  0.833,
                            0.140,  0.616,  0.489,
                            0.997,  0.513,  0.064,
                            0.945,  0.719,  0.592,
                            0.543,  0.021,  0.978,
                            0.279,  0.317,  0.505,
                            0.167,  0.620,  0.077,
                            0.347,  0.857,  0.137,
                            0.055,  0.953,  0.042,
                            0.714,  0.505,  0.345,
                            0.783,  0.290,  0.734,
                            0.722,  0.645,  0.174,
                            0.302,  0.455,  0.848,
                            0.225,  0.587,  0.040,
                            0.517,  0.713,  0.338,
                            0.053,  0.959,  0.120,
                            0.393,  0.621,  0.362,
                            0.673,  0.211,  0.457,
                            0.820,  0.883,  0.371,
                            0.982,  0.099,  0.879]
    vertexBufferName <- fillNewBuffer vertexBufferData
    colorBufferId <- fillNewBuffer colorBufferData

    --Drawing
    GLFW.swapInterval 1
    ( do
        t0 <- fromJust <$> GLFW.getTime -- Get initial time
        glClear gl_COLOR_BUFFER_BIT -- Clear color buffer
        glClear gl_DEPTH_BUFFER_BIT
        glUseProgram programID
        with (mvpMatrix ) $
          glUniformMatrix4fv mvpID 1 (fromBool True) . castPtr
        bindBufferToAttrib vertexBufferName vertexPosition_modelspaceID
        bindBufferToAttrib colorBufferId vertexColor
        glDrawArrays gl_TRIANGLES 0 (12 *3) -- The drawing!
        glDisableVertexAttribArray vertexPosition_modelspaceID
        glDisableVertexAttribArray vertexColor
        GLFW.swapBuffers window
        GLFW.pollEvents
        t1 <- fromJust <$> GLFW.getTime -- Final time
        let deltaT = realToFrac $ t1 - t0 :: Double -- Calculate the time difference
        threadDelay $ if deltaT > 1 / 60
            then 0
            else round $ 1000000 * ((1 / 60) - deltaT) -- Delays the required amount to stay at 60 FPS
        t2 <- fromJust <$> GLFW.getTime -- Time after the delay
        putStrLn $ show ((1 :: Double) / realToFrac (t1 - t0)) -- FPS of the loop
                ++ " -> "
                ++ show ((1 :: Double) / realToFrac (t2 - t0)) -- FPS with the delay
     ) `untilM_` ((||) <$> ((==) GLFW.KeyState'Pressed <$> GLFW.getKey window GLFW.Key'Escape)
                       <*> GLFW.windowShouldClose window) -- Chech for ESC or window close
    -- Finalizating
    alloca $ \ptr -> do
      glDeleteBuffers 1 ptr
    glDeleteVertexArrays 1 vertexArrayIDPtr
    glDeleteProgram programID
    shutdown window