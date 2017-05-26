{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

import Control.Monad
import System.IO
import System.Directory

import Data.Vect
import Graphics.UI.GLFW as GLFW

import LambdaCube.Compiler as LambdaCube -- compiler
import LambdaCube.GL as LambdaCubeGL -- renderer
import qualified LambdaCube.OBJ as OBJ
import Logic

asteroidsModificationTime = getModificationTime "Asteroids02.lc"

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
            "position"        @: V3F
            "angle"           @: Float

    storage <- LambdaCubeGL.allocStorage inputSchema

    -- load OBJ geometry and material descriptions
    let loadObj fname = OBJ.loadOBJToGPU fname >>= \case
          Left err -> fail err
          Right a -> return a
    (asteroidMesh,asteroidMtl) <- loadObj "data/asteroid.obj"
    (spaceshipMesh,spaceshipMtl) <- loadObj "data/spaceship.obj"

    -- load materials textures
    gpuMtlLib <- OBJ.uploadMtlLib $ mconcat [spaceshipMtl, asteroidMtl]

    asteroidObj <- OBJ.addOBJToObjectArray storage "objects" [] asteroidMesh gpuMtlLib
    spaceshipObj <- OBJ.addOBJToObjectArray storage "objects" ["position", "angle"] spaceshipMesh gpuMtlLib

    -- allocate GL pipeline
    let loadRenderer = do
          LambdaCube.compileMain ["."] OpenGL33 "Asteroids02.lc" >>= \case
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

        disableObjects :: [LambdaCubeGL.Object] -> IO ()
        disableObjects objs = mapM_ (\obj -> LambdaCubeGL.enableObject obj False) objs

        addToScene :: Vec2 -> Float -> V4 Float -> [LambdaCubeGL.Object] -> IO ()
        addToScene position angle color objs = do
          forM_ objs $ \obj -> do
            LambdaCubeGL.enableObject obj True
            LambdaCubeGL.updateObjectUniforms obj $ do
              "angle" @= return angle
              "diffuseColor" @= return color
              "position" @= let Vec2 x y = position in do
                return (V3 x y 0)

        white = V4 1 1 1 1 :: V4 Float

        floatTime :: IO Float
        floatTime = do
          Just t <- GLFW.getTime
          return $ realToFrac t

        loop renderer world@World{..} t0 lcTime0 = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                t <- floatTime
                LambdaCubeGL.updateUniforms storage $ do
                  "time" @= return t

                case spaceship of
                  Nothing -> disableObjects spaceshipObj
                  Just spaceship -> void $ addToScene (sPosition spaceship) (sAngle spaceship) white spaceshipObj

                -- render
                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k

                userInput <- UserInput
                  <$> keyIsPressed Key'Left
                  <*> keyIsPressed Key'Right
                  <*> keyIsPressed Key'Up
                  <*> keyIsPressed Key'Down
                  <*> keyIsPressed Key'Space
                  <*> keyIsPressed Key'Enter
                let deltaTime = t - t0
                world' <- stepGame deltaTime userInput world -- Hint: IO is instance of MonadRandom

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
                if escape then return () else loop renderer' world' t lcTime

    Just renderer <- loadRenderer
    t0 <- floatTime
    lcTime <- asteroidsModificationTime
    loop renderer world0 t0 lcTime

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
