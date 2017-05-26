{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module LambdaCube.OBJ
  ( objToMesh
  , loadOBJ
  , loadOBJToGPU
  , uploadMtlLib
  , addOBJToObjectArray
  ) where

import Data.Text (unpack,Text)
import Data.List (groupBy,nub)
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V

import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL

import Codec.Picture as Juicy
import Codec.Wavefront

import LambdaCube.MtlParser

objToMesh :: WavefrontOBJ -> [(Mesh,Maybe Text)]
objToMesh WavefrontOBJ{..} = [(toMesh faceGroup, elMtl . head $ faceGroup) | faceGroup <- faces] where
  faces = groupBy (\a b -> elMtl a == elMtl b) (V.toList objFaces)
  toMesh l = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V4F position)
        , ("normal",    A_V3F normal)
        , ("uvw",       A_V3F texcoord)
        ]
    , mPrimitive    = P_Triangles
    } where
        triangulate (Triangle a b c) = [a,b,c]
        triangulate (Quad a b c d) = [a,b,c, c,d,a]
        triangulate (Face a b c l) = a : b : c : concatMap (\(x,y) -> [a,x,y]) (zip (c:l) l) -- should work for convex polygons without holes
        defaultPosition = Location 0 0 0 0
        defaultNormal = Normal 0 0 0
        defaultTexCoord = TexCoord 0 0 0
        v !- i = v V.!? (i-1)
        toVertex FaceIndex{..} = ( let Location x y z w = fromMaybe defaultPosition (objLocations !- faceLocIndex)            in V4 x y z w
                                 , let Normal x y z     = fromMaybe defaultNormal ((objNormals !-) =<< faceNorIndex)          in V3 x y z
                                 , let TexCoord x y z   = fromMaybe defaultTexCoord ((objTexCoords !-) =<< faceTexCoordIndex) in V3 x y z
                                 )
        (position,normal,texcoord) = V.unzip3 . V.concat . map (V.fromList . map toVertex . triangulate . elValue) $ l


loadOBJ :: String -> IO (Either String ([(Mesh,Maybe Text)],MtlLib))
loadOBJ fname = fromFile fname >>= \case -- load geometry
  Left err -> putStrLn err >> return (Left $ "wavefront error: " ++ err)
  Right obj@WavefrontOBJ{..} -> do
    -- load materials
    mtlLib <- mconcat . V.toList <$> mapM (readMtl . unpack) objMtlLibs
    return $ Right (objToMesh obj,mtlLib)

loadOBJToGPU :: String -> IO (Either String ([(GPUMesh, Maybe Text)], MtlLib))
loadOBJToGPU fname = loadOBJ fname >>= \case
  Left err -> return $ Left err
  Right (subModels,mtlLib) -> do
    gpuSubModels <- forM subModels $ \(mesh,mat) -> LambdaCubeGL.uploadMeshToGPU mesh >>= \a -> return (a,mat)
    return $ Right (gpuSubModels,mtlLib)

uploadMtlLib :: MtlLib -> IO (Map Text (ObjMaterial,TextureData))
uploadMtlLib mtlLib = do
  -- collect used textures
  let usedTextures = nub . concatMap (maybeToList . mtl_map_Kd) $ Map.elems mtlLib
      whiteImage = Juicy.ImageRGB8 $ Juicy.generateImage (\_ _ -> Juicy.PixelRGB8 255 255 255) 1 1
      checkerImage = Juicy.ImageRGB8 $ Juicy.generateImage (\x y -> if mod (x + y) 2 == 0 then Juicy.PixelRGB8 0 0 0 else Juicy.PixelRGB8 255 255 0) 2 2
  checkerTex <- LambdaCubeGL.uploadTexture2DToGPU checkerImage
  -- load images and upload to gpu
  textureLib <- forM (Map.fromList $ zip usedTextures usedTextures) $ \fname -> Juicy.readImage fname >>= \case
    Left err  -> putStrLn err >> return checkerTex
    Right img -> LambdaCubeGL.uploadTexture2DToGPU img
  whiteTex <- LambdaCubeGL.uploadTexture2DToGPU whiteImage
  -- pair textures and materials
  return $ (\a -> (a, maybe whiteTex (fromMaybe checkerTex . flip Map.lookup textureLib) . mtl_map_Kd $ a)) <$> mtlLib

addOBJToObjectArray :: GLStorage -> String -> [String] -> [(GPUMesh, Maybe Text)] -> Map Text (ObjMaterial,TextureData) -> IO [LambdaCubeGL.Object]
addOBJToObjectArray storage slotName objUniforms objMesh mtlLib = forM objMesh $ \(mesh,mat) -> do
  obj <- LambdaCubeGL.addMeshToObjectArray storage slotName (objUniforms ++ ["diffuseTexture","diffuseColor"]) mesh -- diffuseTexture and diffuseColor values can change on each model
  case mat >>= flip Map.lookup mtlLib of
    Nothing -> return ()
    Just (ObjMaterial{..},t) -> LambdaCubeGL.updateObjectUniforms obj $ do
      "diffuseTexture" @= return t -- set model's diffuse texture
      "diffuseColor" @= let (r,g,b) = mtl_Kd in return (V4 r g b mtl_Tr)
  return obj

