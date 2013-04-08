module Main where

import System.Exit (exitWith, ExitCode(ExitSuccess))

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef (IORef, newIORef)
import Control.Monad

main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow "OpenGL & Haskell"
  displayCallback $= display
  depthFunc $= Just Less
  keyboardMouseCallback $= Just (keyboard)
  initApp
  mainLoop

display :: IO ()
display = do
  clear [ColorBuffer, DepthBuffer]
  polygonSmooth $= Enabled
  drawCube
  swapBuffers

drawCube :: IO ()
drawCube =
  cube3d (1.0::GLfloat)

cube3d w = do
  renderPrimitive Quads $ do
    color $ Color3 0.0 255.0 (0.0::GLfloat)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (w) (-w) (-w)
    vertex $ Vertex3 (w) (w) (-w)
    vertex $ Vertex3 (-w) (w) (-w)
    color $ Color3 255.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 (-w) (-w) (w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (w) (-w)
    vertex $ Vertex3 (-w) (w) (w)
    color $ Color3 0.0 0.0 (255.0::GLfloat)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (w)
    vertex $ Vertex3 (w) (-w) (w)
    vertex $ Vertex3 (w) (-w) (-w)
    color $ Color3 255.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 (w) (-w) (-w)
    vertex $ Vertex3 (w) (-w) (w)
    vertex $ Vertex3 (w) (w) (w)
    vertex $ Vertex3 (w) (w) (-w)
    color $ Color3 0.0 0.0 (255.0::GLfloat)
    vertex $ Vertex3 (-w) (w) (-w)
    vertex $ Vertex3 (-w) (w) (w)
    vertex $ Vertex3 (w) (w) (w)
    vertex $ Vertex3 (w) (w) (-w)
    color $ Color3 0.0 255.0 (0.0::GLfloat)
    vertex $ Vertex3 (-w) (-w) (w)
    vertex $ Vertex3 (w) (-w) (w)
    vertex $ Vertex3 (w) (w) (w)
    vertex $ Vertex3 (-w) (w) (w)

initApp :: IO ()
initApp = do
  matrixMode $= Projection
  perspective 40.0 1.0 1.0 10.0
  matrixMode $= Modelview 0
  lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)

keyboard (Char c) _ _ _ = case c of
  'e' -> rt (-1.0) 0.0 0.0
  'd' -> rt 1.0 0.0 0.0
  's' -> rt 0.0 (-1.0) 0.0
  'f' -> rt 0.0 (1.0) 0.0
  '\27' -> exitWith ExitSuccess
  _     -> return ()
  where rt x 0.0 0.0 = do
          rotate angle ((Vector3 x 0.0 0.0)::Vector3 GLfloat)
          postRedisplay Nothing
        rt 0.0 y 0.0 = do
          rotate angle ((Vector3 0.0 y 0.0)::Vector3 GLfloat)
          postRedisplay Nothing
        rt 0.0 0.0 z = do
          rotate angle ((Vector3 0.0 0.0 z)::Vector3 GLfloat)
          postRedisplay Nothing
        angle = 10

keyboard _ _ _ _ = do
  return ()