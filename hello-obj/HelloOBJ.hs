{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import System.Environment
import Graphics.UI.GLFW as GLFW

import LambdaCube.Compiler as LambdaCube -- compiler
import LambdaCube.GL as LambdaCubeGL -- renderer

import LambdaCube.OBJ

----------------------------------------------------
--  See:  http://lambdacube3d.com/getting-started
----------------------------------------------------

main :: IO ()
main = do
    -- compile hello.lc to graphics pipeline description
    pipelineDesc <- LambdaCube.compileMain ["."] OpenGL33 "hello_obj.lc" >>= \case
      Left err  -> fail $ "compile error:\n" ++ ppShow err
      Right pd  -> return pd

    win <- initWindow "LambdaCube 3D DSL OBJ viewer" 640 640

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

    objName <- head . (++ ["cube.obj"]) <$> getArgs
    -- load OBJ geometry and material descriptions
    Right (objMesh,mtlLib) <- loadOBJToGPU objName
    -- load materials textures
    gpuMtlLib <- uploadMtlLib mtlLib
    -- add OBJ to pipeline input
    addOBJToObjectArray storage "objects" [] objMesh gpuMtlLib

    -- allocate GL pipeline
    renderer <- LambdaCubeGL.allocRenderer pipelineDesc
    LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
      Just err -> putStrLn err
      Nothing  -> loop
        where loop = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                LambdaCubeGL.updateUniforms storage $ do
                  "time" @= do
                              Just t <- GLFW.getTime
                              return (realToFrac t :: Float)
                -- render
                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed Key'Escape
                if escape then return () else loop

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
