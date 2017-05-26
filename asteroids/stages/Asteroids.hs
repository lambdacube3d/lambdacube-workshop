{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

import Control.Monad
import System.IO
import System.Directory

import Graphics.UI.GLFW as GLFW

import LambdaCube.Compiler as LambdaCube -- compiler
import LambdaCube.GL as LambdaCubeGL -- renderer

asteroidsModificationTime = getModificationTime "Asteroids.lc"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    win <- initWindow "LambdaCube Asteroids" 640 640

    -- setup render data
    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V4F
            "normal"    @: Attribute_V3F
            "uvw"       @: Attribute_V3F
          defUniforms $ do
            "time"            @: Float
            "diffuseTexture"  @: FTexture2D
            "diffuseColor"    @: V4F

    storage <- LambdaCubeGL.allocStorage inputSchema

    -- allocate GL pipeline
    let loadRenderer = do
          LambdaCube.compileMain ["."] OpenGL33 "Asteroids.lc" >>= \case
            Left err  -> do
              putStrLn $ "compile error:\n" ++ ppShow err
              return Nothing
            Right pipelineDesc -> do
              renderer <- LambdaCubeGL.allocRenderer pipelineDesc
              LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
                Just err -> do
                  putStrLn $ "setStorage error: " ++ err
                  LambdaCubeGL.disposeRenderer renderer
                  return Nothing
                Nothing -> do
                  putStrLn $ "setStorage ok"
                  return $ Just renderer

        floatTime :: IO Float
        floatTime = do
          Just t <- GLFW.getTime
          return $ realToFrac t

        loop renderer t0 lcTime0 = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                t <- floatTime
                LambdaCubeGL.updateUniforms storage $ do
                  "time" @= return t

                -- render
                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k

                lcTime <- asteroidsModificationTime
                let reload = lcTime /= lcTime0
                renderer' <- if not reload then return renderer else do
                  loadRenderer >>= \case
                    Nothing -> return renderer
                    Just newRenderer -> do
                      putStrLn "Reload renderer"
                      LambdaCubeGL.disposeRenderer renderer
                      return newRenderer
                t <- if reload then floatTime else return t
                escape <- keyIsPressed Key'Escape
                if escape then return () else loop renderer' t lcTime

    Just renderer <- loadRenderer
    t0 <- floatTime
    lcTime <- asteroidsModificationTime
    loop renderer t0 lcTime

    LambdaCubeGL.disposeRenderer renderer
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
