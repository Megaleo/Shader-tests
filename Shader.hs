{-# LANGUAGE ImplicitParams, RecordWildCards,
             NoMonomorphismRestriction #-}

module Shader where

import System.IO
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal

withNewPtr :: Storable a => (Ptr a -> IO b) -> IO a
withNewPtr f = alloca (\ptr -> f ptr >> peek ptr)

checkStatus :: (Integral a1, Storable a1)
    => GLenum
    -> (t -> GLenum -> Ptr a1 -> IO a)
    -> (t -> a1 -> Ptr a3 -> Ptr Foreign.C.Types.CChar -> IO a2)
    -> t
    -> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn componentId = do
    let
        fetch info = withNewPtr (glGetFn componentId info)
    status <- liftM toBool $ fetch statusFlag
    logLength <- fetch gl_INFO_LOG_LENGTH
    when (logLength > 0) $
        allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
            _ <- glInfoLogFn componentId logLength nullPtr msgPtr
            msg <- peekCString msgPtr
            (if status then putStrLn else fail) msg
    return status

loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderTypeFlag filePath = do
    code <- readFile filePath
    shaderId <- glCreateShader shaderTypeFlag
    withCString code $ \codePtr ->
        with codePtr $ \codePtrPtr ->
            glShaderSource shaderId 1 codePtrPtr nullPtr
    putStrLn $ "Compiling shader : " ++ code
    glCompileShader shaderId
    _ <- checkStatus
        gl_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog shaderId
    return shaderId

loadProgram :: FilePath -> FilePath -> IO GLuint
loadProgram vertFP fragFP = do
  shaderIds <- mapM (uncurry loadShader)
    [(gl_VERTEX_SHADER, vertFP)
    ,(gl_FRAGMENT_SHADER, fragFP)]
  progId <- glCreateProgram
  putStrLn "Linking Program"
  mapM_ (glAttachShader progId) shaderIds
  glLinkProgram progId
  checkStatus gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
  mapM_ glDeleteShader shaderIds
  return progId