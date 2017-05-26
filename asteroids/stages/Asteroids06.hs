{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

import Control.Monad
import System.IO
import System.Directory

import Data.Vect
import Codec.Picture as Juicy
import Graphics.UI.GLFW as GLFW

import LambdaCube.Compiler as LambdaCube -- compiler
import LambdaCube.GL as LambdaCubeGL -- renderer
import qualified LambdaCube.OBJ as OBJ
import Logic

-- for mesh construction
import LambdaCube.GL.Mesh as LambdaCubeGL
import qualified Data.Map as Map
import qualified Data.Vector as V

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
          defObjectArray "debugObjects" Triangles $ do
            "position"  @: Attribute_V3F
          defObjectArray "quad" Triangles $ do
            "position"  @: Attribute_V2F
            "uv"        @: Attribute_V2F
          defUniforms $ do
            "time"            @: Float
            "diffuseTexture"  @: FTexture2D
            "diffuseColor"    @: V4F
            "position"        @: V3F
            "angle"           @: Float
            "radius"          @: Float

    storage <- LambdaCubeGL.allocStorage inputSchema

    -- load OBJ geometry and material descriptions
    let loadObj fname = OBJ.loadOBJToGPU fname >>= \case
          Left err -> fail err
          Right a -> return a
    (asteroidMesh,asteroidMtl)   <- loadObj "data/asteroid.obj"
    (spaceshipMesh,spaceshipMtl) <- loadObj "data/spaceship.obj"
    (bulletMesh,bulletMtl)       <- loadObj "data/missile.obj"

    let objRadius obj = maximum [boundingSphereRadius $ meshData mesh | (mesh, _) <- obj]
        spaceshipRadius = objRadius spaceshipMesh
        asteroidRadius = objRadius asteroidMesh
        bulletRadius = objRadius bulletMesh

    putStrLn $ "spaceship radius: " ++ show spaceshipRadius
    putStrLn $ "asteroid radius: " ++ show asteroidRadius
    putStrLn $ "bullet radius: " ++ show bulletRadius

    -- load materials textures
    gpuMtlLib <- OBJ.uploadMtlLib $ mconcat [spaceshipMtl, asteroidMtl, bulletMtl]

    spaceshipObj <- OBJ.addOBJToObjectArray storage "objects" ["position", "angle", "radius"] spaceshipMesh gpuMtlLib
    asteroidPool <- replicateM 1000 $ OBJ.addOBJToObjectArray storage "objects" ["position", "angle", "radius"] asteroidMesh gpuMtlLib
    bulletPool   <- replicateM 1000 $ OBJ.addOBJToObjectArray storage "objects" ["position", "angle", "radius"] bulletMesh gpuMtlLib

    -- debug sphere
    sphereMesh <- LambdaCubeGL.uploadMeshToGPU $ sphere 1 8
    spherePool <- replicateM 1000 $ do
      sphereObj <- LambdaCubeGL.addMeshToObjectArray storage "debugObjects" ["diffuseColor", "position", "radius", "angle"] sphereMesh
      return [sphereObj]

    -- background
    quadMesh <- LambdaCubeGL.uploadMeshToGPU quad
    backgroundObj <- LambdaCubeGL.addMeshToObjectArray storage "quad" ["diffuseTexture"] quadMesh

    -- load image and upload texture
    Right img <- Juicy.readImage "data/background.jpg"
    textureData <- LambdaCubeGL.uploadTexture2DToGPU img

    LambdaCubeGL.updateObjectUniforms backgroundObj $ do
      "diffuseTexture" @= return textureData

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

        disableObjects :: [LambdaCubeGL.Object] -> IO ()
        disableObjects objs = mapM_ (\obj -> LambdaCubeGL.enableObject obj False) objs

        addToScene :: Vec2 -> Float -> Float -> V4 Float -> [[LambdaCubeGL.Object]] -> IO [[LambdaCubeGL.Object]]
        addToScene position angle radius color (objs:pool) = do
          forM_ objs $ \obj -> do
            LambdaCubeGL.enableObject obj True
            LambdaCubeGL.updateObjectUniforms obj $ do
              "angle" @= return angle
              "radius" @= return radius
              "diffuseColor" @= return color
              "position" @= let Vec2 x y = position in do
                return (V3 x y 0)
          return pool

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

                -- collision visuals
                spherePool1 <- case spaceship of
                  Just spaceship -> addToScene (sPosition spaceship) (sAngle spaceship) (sRadius spaceship) (V4 1 0 0 1) spherePool
                  _ -> return spherePool
                spherePool2 <- foldM (\pool asteroid -> addToScene (aPosition asteroid) 0 (aRadius asteroid) (V4 0 1 0 1) pool) spherePool1 asteroids
                spherePool3 <- foldM (\pool bullet   -> addToScene (bPosition bullet)   0 (bRadius bullet)   (V4 0 1 1 1) pool) spherePool2 bullets
                disableObjects $ concat spherePool3

                -- spaceship
                case spaceship of
                  Nothing -> disableObjects spaceshipObj
                  Just spaceship -> void $ addToScene (sPosition spaceship) (sAngle spaceship) (sRadius spaceship / spaceshipRadius) white [spaceshipObj]

                -- asteroids
                asteroidPool1 <- foldM (\pool asteroid -> addToScene (aPosition asteroid) 0 (aRadius asteroid / asteroidRadius) white pool) asteroidPool asteroids
                disableObjects $ concat asteroidPool1

                -- bullets
                bulletPool1 <- foldM (\pool Bullet{..} -> addToScene bPosition bAngle (bRadius / bulletRadius) white pool) bulletPool bullets
                disableObjects $ concat bulletPool1

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

-- utils
sphere :: Float -> Int -> LambdaCubeGL.Mesh
sphere radius n = Mesh
    { mAttributes = Map.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_TrianglesI indices
    }
  where
    m = pi / fromIntegral n
    vertices = V.map (\(V3 x y z) -> V3 (radius * x) (radius * y) (radius * z)) normals
    normals = V.fromList [V3 (sin a * cos b) (cos a) (sin a * sin b) | i <- [0..n], j <- [0..2 * n - 1],
                          let a = fromIntegral i * m, let b = fromIntegral j * m]
    indices = V.fromList $ concat [[ix i j, ix i' j, ix i' j', ix i' j', ix i j', ix i j] | i <- [0..n - 1], j <- [0..2 * n - 1],
                                   let i' = i + 1, let j' = (j + 1) `mod` (2 * n)]
    ix i j = fromIntegral (i * 2 * n + j)

v4ToVec3 :: V4 Float -> Vec3
v4ToVec3 (V4 x y z _) = Vec3 x y z

boundingSphereRadius :: LambdaCubeGL.Mesh -> Float
boundingSphereRadius Mesh{..} = case Map.lookup "position" mAttributes of
  Just (A_V4F vertices) -> maximum $ fmap (len . v4ToVec3) vertices
  _ -> 0

quad :: LambdaCubeGL.Mesh
quad = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 1 (-1), V2 (-1) (-1)
                                           ,V2 1 1, V2 (-1) (-1), V2 (-1) 1])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 1 0, V2 0 0
                                           ,V2 1 1, V2 0 0, V2 0 1])
        ]
    , mPrimitive    = P_Triangles
    }
