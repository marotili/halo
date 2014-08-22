{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Render.SpriteManager
    ( Sprite
    , SpriteInstance
    , newSprite
    , Image
    , newImage
    , SpriteManager
    , SpriteRenderUnit
    , SpriteRenderer
    , newSpriteManager
    , newSpriteRenderer
    , newSpriteRenderUnit
    , newSpriteInstance
    , renderSprites

    , setSpriteInstances
    , uploadRenderUnit

    , siPosition
    , siRotation

    , saSprites
    , smAtlas
    ) where

import Debug.Trace
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.Rendering.OpenGL (($=))
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Control.Monad.Primitive
import Control.Monad.ST

import Foreign.C.Types

import Control.Monad.Morph (hoist, generalize)
import Control.Monad.Identity

import qualified Data.Map as Map
import Control.Lens ((^.), _Just, makeLenses, use, at, (.=), Getter, to, _1, _2, (%=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Monad.State.Strict

import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
-- | float to word conversion + word32
import Data.Binary.IEEE754
import Data.Word

import Foreign.Ptr

-- | gl buffer upload
import Render.Core.Render
import Render.Core.Error
import Render.Core.Camera (Camera, programSetViewProjection)
import Render.Core.Manager (RenderManager, mkTextureUnit)

type SpriteAtlasName = String
type SpriteName = String

type NumEntities = Int

-- | todo: maybe the sprite data type can reference the atlas instread of the atlas name
data Sprite = Sprite
    { _spriteName :: SpriteName
    , _spriteAtlasName :: SpriteAtlasName
    , _spriteOffset :: (Int, Int)
    , _spriteSize :: (Int, Int)
    , _spriteOrigin :: (Int, Int)
    } deriving (Show)

data SpriteInstance = SpriteInstance
    { _siSprite :: Sprite
    , _siPosition :: (Float, Float)
    , _siRotation :: Float
    } deriving (Show)

-- | constructor
newSpriteInstance :: Sprite -> (Float, Float) -> Float -> SpriteInstance
newSpriteInstance = SpriteInstance

-- | constructor
newSprite :: SpriteName -> String -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Sprite
newSprite = Sprite

-- | one image set
data SpriteAtlas = SpriteAtlas
    { _saName :: String
    , _saSprites :: Map.Map SpriteName Sprite
    , _saImage :: Image
    } deriving (Show)

-- | constructor
newSpriteAtlas :: SpriteAtlasName -> Image -> SpriteAtlas
newSpriteAtlas name = SpriteAtlas name Map.empty

data Image = Image
    { _iSource :: FilePath
    , _iWidth, _iHeight :: Int
    } deriving (Eq, Show)

-- | constructor
newImage :: FilePath -> Int -> Int -> Image
newImage = Image

-- | stores all avaiable sprites
data SpriteManager = SpriteManager
    { _smAtlas :: Map.Map SpriteAtlasName SpriteAtlas
    } deriving (Show)

-- | uploaded image info
data SpriteTextureInfo = SpriteTextureInfo
    { _stiTexUnit :: GL.TextureUnit
    , _stiAtlasName :: String
    , _stiTexture :: GL.TextureObject
    } deriving (Show)

-- | a group of uploaded sprite atlass'
data SpriteAtlasGroup = SpriteAtlasGroup
    { _sagTextures :: Map.Map SpriteAtlasName SpriteTextureInfo
    } deriving (Show)

type RenderUnitId = Int

-- | render controller for sprites (they share the program and textures)
data SpriteRenderer = SpriteRenderer
    { _srTextures :: !SpriteAtlasGroup
    , _srManager :: !SpriteManager
    , _srProgram :: !GL.Program
    } deriving (Show)

-- | a single render pass (e.g. layer)
data SpriteRenderUnit = SpriteRenderUnit
    { _sruVAO :: !GL.VertexArrayObject
    , _sruBuffer :: !SpriteBuffer
    } deriving (Show)

-- | buffer helper for a single render pass
data SpriteBuffer = SpriteBuffer
    { _sbGLBuffer :: !GL.BufferObject
    , _sbElementBuffer :: !GL.BufferObject
    , _sbGLBufferSize :: !Int -- | size in 4 bytes * 5
    , _sbData :: !(V.Vector Word32) -- | (Position, Origin, Size, Rotation, TextureUnit)
    , _sbElementData :: !(V.Vector CUInt)
    , _sbUsedSize :: !Int
    } deriving (Show)

-- | number of indizes used to render
sbRenderElementLength :: Getter SpriteBuffer Int
sbRenderElementLength = to (\sb -> _sbUsedSize sb * 6)

data SpriteBufferElement = SpriteBufferElement
    { _sbePosition :: (Float, Float)
    , _sbeOrigin :: (Float, Float)
    , _sbeRotation :: Float
    , _sbeTextureUnit :: Int -- |starts with zero (index not textureUnit!)
    , _sbeTexCoords :: (Float, Float)
    , _sbeMesh :: (Float, Float)
    , _sbePadding :: (Float, Float, Float, Float, Float, Float)
    } deriving (Show)

makeLenses ''SpriteBufferElement
makeLenses ''SpriteBuffer
makeLenses ''Sprite
makeLenses ''SpriteAtlas
makeLenses ''SpriteAtlasGroup
makeLenses ''Image
makeLenses ''SpriteManager
makeLenses ''SpriteRenderUnit
makeLenses ''SpriteRenderer
makeLenses ''SpriteInstance
makeLenses ''SpriteTextureInfo

--newSpriteTextureInfo :: SpriteTextureInfo
--newSpriteTextureInfo = SpriteTextureInfo
--    { _stiTexUnit
    --}

-- | create a new sprite manager (loads json file from hardcoded file tileset_compiled.json)
newSpriteManager :: IO SpriteManager
newSpriteManager = do
    file <- loadSprites

    return $ execState (loadComplexTilesets file) SpriteManager
        { _smAtlas = Map.empty
        }

-- | create a new renderer (shader, upload textures)
-- hardcoded: shader (sprite.vert, sprite.frag)
newSpriteRenderer :: SpriteManager -> StateT RenderManager IO SpriteRenderer
newSpriteRenderer sm = do
    program <- lift $ setupShaders "sprite.vert" "sprite.frag"
    lift $ GL.currentProgram $= Just program
    lift $ logGL "currentProgram"

    atlasGroup <- initTextures sm

    return SpriteRenderer
        { _srTextures = SpriteAtlasGroup atlasGroup
        , _srManager = sm
        , _srProgram = program
        }

    where
        initTextures sm = traceShow (sm^.smAtlas) $ do
            let numImages = Map.size $ sm^.smAtlas
            textureUnits <- replicateM numImages (hoist generalize mkTextureUnit)
            textures <- lift (GL.genObjectNames numImages :: IO [GL.TextureObject])
            lift $ logGL "newSpriteRenderer: genObjectNames"

            foldM (\atlasGroup (texUnit, texture, spriteAtlasName) -> lift $ do
                    traceShow ("start") $ return ()
                    let Just spriteAtlas = sm^.smAtlas.at spriteAtlasName
                    Right pngImage <- traceShow (spriteAtlas^.saImage.iSource) $ P.readPng $ spriteAtlas^.saImage.iSource
                    uploadImage texUnit texture pngImage
                    traceShow ("test") $ return $ Map.insert spriteAtlasName
                        (SpriteTextureInfo texUnit spriteAtlasName texture)
                        atlasGroup

                ) Map.empty (zip3 textureUnits textures (Map.keys $ sm^.smAtlas))

-- | empty group
emptySpriteAtlasGroup :: SpriteAtlasGroup
emptySpriteAtlasGroup = SpriteAtlasGroup Map.empty

-- | create a new render unit
-- sets up vertex array objects and initializes buffer
newSpriteRenderUnit :: SpriteRenderer -> NumEntities -> IO SpriteRenderUnit
newSpriteRenderUnit sr numEntities = do
    [vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
    logGL "newSpriteRenderUnit: genObjectNames"
    buf <- newDynamicSpriteBuffer numEntities
    bindSpriteBuffer vao buf

    return SpriteRenderUnit
        { _sruVAO = vao
        , _sruBuffer = buf
        }

    where
        bindSpriteBuffer vao buf = do
            let program = sr^.srProgram
            GL.currentProgram $= Just program
            GL.bindVertexArrayObject $= Just vao
            GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
            GL.AttribLocation originLoc <- GL.get $ GL.attribLocation program "origin"
            GL.AttribLocation rotationLoc <- GL.get $ GL.attribLocation program "rotation"
            GL.AttribLocation imageLoc <- GL.get $ GL.attribLocation program "image"
            GL.AttribLocation texCoordsLoc <- GL.get $ GL.attribLocation program "texCoords"
            GL.AttribLocation meshLoc <- GL.get $ GL.attribLocation program "mesh"
            logGL "attrib locations"

            GL.bindBuffer GL.ArrayBuffer $= Just (buf^.sbGLBuffer)
            logGL "newSpriteRenderUnit: bindBuffer"
            let stride = 64
            GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 stride nullPtr
            logGL "newSpriteRenderUnit: attribPointer posLoc"
            GLRaw.glVertexAttribPointer originLoc 2 GLRaw.gl_FLOAT 0 stride (plusPtr nullPtr 8)
            logGL "newSpriteRenderUnit: attribPointer originLoc"
            GLRaw.glVertexAttribPointer texCoordsLoc 2 GLRaw.gl_FLOAT 0 stride (plusPtr nullPtr 16)
            logGL "newSpriteRenderUnit: attribPointer texCoordsLoc"
            GLRaw.glVertexAttribPointer meshLoc 2 GLRaw.gl_FLOAT 0 stride (plusPtr nullPtr 24)
            logGL "newSpriteRenderUnit: attribPointer meshLoc"

            GLRaw.glVertexAttribPointer rotationLoc 1 GLRaw.gl_FLOAT 0 stride (plusPtr nullPtr 32)
            logGL "newSpriteRenderUnit: attribPointer rotationLoc"

            GLRaw.glVertexAttribIPointer imageLoc 1 GLRaw.gl_INT stride (plusPtr nullPtr 36)
            logGL "newSpriteRenderUnit: attribPointer imageLoc"

            GLRaw.glEnableVertexAttribArray posLoc
            logGL "newSpriteRenderUnit: enable posLoc"
            GLRaw.glEnableVertexAttribArray originLoc
            logGL "newSpriteRenderUnit: enable posLoc"
            GLRaw.glEnableVertexAttribArray rotationLoc
            logGL "newSpriteRenderUnit: enable rotationLoc"
            GLRaw.glEnableVertexAttribArray imageLoc
            logGL "newSpriteRenderUnit: enable imageLoc"
            GLRaw.glEnableVertexAttribArray texCoordsLoc
            logGL "newSpriteRenderUnit: enable texCoordsLoc"
            GLRaw.glEnableVertexAttribArray meshLoc
            logGL "newSpriteRenderUnit: enable meshLoc"

            logGL "vertex attributes set"

-- | render sprites
-- TODO: maybe remove camera
renderSprites :: SpriteRenderer -> SpriteRenderUnit -> Camera -> IO ()
renderSprites sr sru cam = do
    let program = sr^.srProgram
    GL.currentProgram $= Just program
    mapM_ (\(i, texInfo) -> do
            sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
            GL.uniform sampler $= (texInfo^.stiTexUnit)
        ) $ zip [0..Map.size (sr^.srTextures.sagTextures) - 1] (Map.elems (sr^.srTextures.sagTextures))

    programSetViewProjection program cam 
    GL.bindVertexArrayObject $= Just (sru^.sruVAO)
    logGL "renderSprites: bindVertexArrayObject"
    GL.bindBuffer GL.ElementArrayBuffer $= Just (sru^.sruBuffer.sbElementBuffer)
    logGL "renderSprites: bindBuffer element"
    GLRaw.glDrawElements GLRaw.gl_TRIANGLES (fromIntegral $ sru^.sruBuffer.sbRenderElementLength) GLRaw.gl_UNSIGNED_INT nullPtr
    logGL "renderSprites: drawElements"
    return ()


-- | number of elements in one buffer element (every element must have 4 byte size)
sbeElementSize :: Int
sbeElementSize = 16 -- | 10 + 6 padding
elementsPerEntity = 4

-- | add a sprite to the sprite manager
smAddTile :: String -> Sprite -> StateT SpriteManager Identity ()
smAddTile name sprite = do
    mAtlas <- use $ smAtlas.at name
    case mAtlas of
        Nothing -> error "atlas does not exist"
        Just _ ->
            smAtlas.at name._Just.saSprites.at (sprite^.spriteName) .= Just sprite

-- | import sprite infos from json file
instance A.FromJSON (StateT SpriteManager Identity ()) where
  parseJSON (A.Object v) = do
    tileSets <- v A..: "tileset"
    foldM loadTileSet (return ()) tileSets

    where
      loadTileSet worldState (A.Object v2) = do
        tsName <- v2 A..: "name"
        tsFilename <- fmap FP.fromText (v2 A..: "filename")
        tsBaseDir <- fmap FP.fromText (v2 A..: "base_directory")
        tsImageWidth <- v2 A..: "imageWidth"
        tsImageHeight <- v2 A..: "imageHeight"
        tsTiles <- v2 A..: "data"

        let Right filename = FP.toText $ tsBaseDir FP.</> tsFilename

        let newWorldState = smAtlas . at tsName .= (Just $ newSpriteAtlas tsName $
              newImage (T.unpack filename) tsImageWidth tsImageHeight
              )

        newWorldState' <- foldM (loadTiles tsName) newWorldState tsTiles
        return (worldState >> newWorldState')
      loadTileSet _ _ = mzero

      loadTiles name worldState (A.Object v3) = do
        width <- v3 A..: "width"
        height <- v3 A..: "height"
        posX <- v3 A..: "offset_x"
        posY <- v3 A..: "offset_y"
        originX <- v3 A..:? "origin_x" :: A.Parser (Maybe Int)
        originY <- v3 A..:? "origin_y" :: A.Parser (Maybe Int)
        tileName <- v3 A..: "tileId"

        return $ worldState >>
            smAddTile name (newSprite tileName name (posX, posY) (width, height) 
              (case originX of Just x -> fromIntegral x; _ -> 0,
               case originY of Just y -> fromIntegral y; _ -> 0))
      loadTiles _ _ _ = mzero
  parseJSON _ = mzero

type LoadTileset = B.ByteString

loadSprites :: IO B.ByteString
loadSprites =
  B.readFile "tileset_compiled.json"

loadComplexTilesets :: B.ByteString -> State SpriteManager ()
loadComplexTilesets input = do
  let updateWorld = A.eitherDecode input :: (Either String (State SpriteManager ()))
  case updateWorld of
    Left err -> error err
    Right update -> update


-- | write buffer element into mutable vector
sbePack :: (PrimMonad m) => SpriteBufferElement -> VM.MVector (PrimState m) Word32 -> m ()
sbePack sbElement vec = do
    VM.write vec 0 (floatToWord (sbElement^.sbePosition._1))
    VM.write vec 1 (floatToWord (sbElement^.sbePosition._2))
    VM.write vec 2 (floatToWord (sbElement^.sbeOrigin._1))
    VM.write vec 3 (floatToWord (sbElement^.sbeOrigin._2))
    VM.write vec 4 (floatToWord (sbElement^.sbeTexCoords._1))
    VM.write vec 5 (floatToWord (sbElement^.sbeTexCoords._2))
    VM.write vec 6 (floatToWord (sbElement^.sbeMesh._1))
    VM.write vec 7 (floatToWord (sbElement^.sbeMesh._2))
    VM.write vec 8 (floatToWord (sbElement^.sbeRotation))
    VM.write vec 9 (fromIntegral (sbElement^.sbeTextureUnit))

    -- padding
    VM.write vec 10 0
    VM.write vec 11 0
    VM.write vec 12 0
    VM.write vec 13 0
    VM.write vec 14 0
    VM.write vec 15 0

-- | getter computes current client buffer size of sprite buffer
sbBufferSize :: Getter SpriteBuffer Int
sbBufferSize = to (V.length . _sbData)

--newStaticSpriteBuffer :: NumEntities -> IO SpriteBuffer
--newStaticSpriteBuffer numEntities = do
--    return ()

-- | test helper without io (no opengl)
newDynamicSpriteBuffer' :: NumEntities -> SpriteBuffer
newDynamicSpriteBuffer' numEntities = SpriteBuffer
        { _sbGLBufferSize = numEntities
        , _sbData = emptyVec
        , _sbElementData = emptyElementVec
        , _sbUsedSize = 0
        , _sbGLBuffer = GL.nullBuffer
        , _sbElementBuffer = GL.nullBuffer
        }
    where
        emptyVec = V.replicate (numEntities*sbeElementSize) 0
        emptyElementVec = V.replicate numEntities 0

-- | initialize a dynamic buffer
newDynamicSpriteBuffer :: NumEntities -> IO SpriteBuffer
newDynamicSpriteBuffer numEntities = do
    [buffer, elementBuffer] <- GL.genObjectNames 2 :: IO [GL.BufferObject]
    logGL "init buffer objects"

    -- | [0, 1, 2, 2, 3, 0]
    let numInElementVec = 6

    -- | entries in vec = 4 per instance * 16 * 4 bytes
    let emptyVec = V.replicate (numEntities*sbeElementSize*elementsPerEntity) 0
    let emptyElementVec = V.replicate (numEntities*numInElementVec) 0

    uploadFromVec (numEntities*sbeElementSize*elementsPerEntity) GL.ArrayBuffer buffer emptyVec
    uploadFromVec (numEntities*numInElementVec) GL.ElementArrayBuffer elementBuffer emptyElementVec
    logGL "upload initial"

    return SpriteBuffer
        { _sbGLBuffer = buffer
        , _sbElementBuffer = elementBuffer
        , _sbGLBufferSize = numEntities
        , _sbData = emptyVec
        , _sbElementData = emptyElementVec
        , _sbUsedSize = 0
        }

-- | upload client side buffer to server side buffer
uploadRenderUnit :: SpriteRenderUnit -> IO ()
uploadRenderUnit sru = do
    updateFromVec GL.ArrayBuffer (sru^.sruBuffer.sbGLBuffer) (sru^.sruBuffer.sbData)
    updateFromVec GL.ElementArrayBuffer (sru^.sruBuffer.sbElementBuffer) (sru^.sruBuffer.sbElementData)


-- | for now we copy the entire data (we receive it sorted by the y-axis)
updateSpriteBuffer :: [SpriteBufferElement] -> State SpriteBuffer ()
updateSpriteBuffer elements = do
    bufferSize <- use sbBufferSize
    if bufferSize < length elements
        then
            error "no resizing implemented"
        else do
            buffer <- use sbData

            let newBuffer = runST $ do
                    modBuffer <- V.unsafeThaw buffer
                    mapM_ (\i -> do
                        let buf = VM.slice (i*sbeElementSize) sbeElementSize modBuffer
                        sbePack (elements!!i) buf
                        ) [0..length elements - 1]
                    V.unsafeFreeze modBuffer

            sbData .= newBuffer

-- | upload a list of sprite instances into client side array buffer
-- TODO: maybe sort here?
setSpriteInstances :: SpriteRenderer -> [SpriteInstance] -> State SpriteRenderUnit ()
setSpriteInstances !sr !instances = do
    let bufferElements = concatMap (_flatten sr) instances
    sruBuffer %= execState (updateSpriteBuffer bufferElements)
    sruBuffer %= execState (mkElementBuffer instances)
    sruBuffer %= execState (sbUsedSize .= (length instances))
    
-- | update the element buffer of a sprite buffer
mkElementBuffer :: [SpriteInstance] -> State SpriteBuffer ()
mkElementBuffer instances = do
    elementBuf <- use sbElementData
    let newElementBuf = runST $ do
            modBuffer <- V.unsafeThaw elementBuf
            let indices = concatMap (\i -> map (+i*4) [0, 1, 2, 2, 3, 0]) [0..length instances - 1]
            mapM_ (\(i, elementIndex) ->
                VM.write modBuffer i (fromIntegral elementIndex)
                ) $ zip [0..] indices
            V.unsafeFreeze modBuffer
    sbElementData .= newElementBuf

-- | flatten sprite instances for upload to gl
_flatten :: SpriteRenderer -> SpriteInstance -> [SpriteBufferElement]
_flatten sr si = 
                        [ SpriteBufferElement pos origin rotation texIndex texCoordsBl meshBl padding
                        , SpriteBufferElement pos origin rotation texIndex texCoordsTl meshTl padding
                        , SpriteBufferElement pos origin rotation texIndex texCoordsTr meshTr padding
                        , SpriteBufferElement pos origin rotation texIndex texCoordsBr meshBr padding
                        ]
  where
      padding = (0, 0, 0, 0, 0, 0)
      atlasName = si^.siSprite.spriteAtlasName
      Just spriteAtlas = sr^.srManager.smAtlas.at atlasName
      Just sprite = spriteAtlas^.saSprites.at (si^.siSprite.spriteName)

      imageWidth = spriteAtlas^.saImage.iWidth
      imageHeight = spriteAtlas^.saImage.iHeight

      pos = (si^.siPosition._1, si^.siPosition._2)
      origin = (fromIntegral $ sprite^.spriteOrigin._1, fromIntegral $ sprite^.spriteOrigin._2)
      rotation = si^.siRotation

      --atlasGroup =
      -- texUnit = sr^?srTextures.sagTextures.at atlasName._Just.stiTexUnit
      texIndex = Map.findIndex atlasName (sr^.srTextures.sagTextures)

      texCoordsBl' = (0, 0)
      texCoordsBl = 
          ( fromIntegral (sprite^.spriteOffset._1) / fromIntegral imageWidth
          , fromIntegral (sprite^.spriteOffset._2 + sprite^.spriteSize._2) / fromIntegral imageHeight)
      texCoordsTl' = (0, 0.2)
      texCoordsTl =
          ( fromIntegral (sprite^.spriteOffset._1) / fromIntegral imageWidth
          , fromIntegral (sprite^.spriteOffset._2) / fromIntegral imageHeight)
      texCoordsTr' = (0.2, 0.2)
      texCoordsTr = 
          ( fromIntegral (sprite^.spriteOffset._1 + sprite^.spriteSize._1) / fromIntegral imageWidth
          , fromIntegral (sprite^.spriteOffset._2) / fromIntegral imageHeight)
      texCoordsBr' = (0.2, 0) 
      texCoordsBr = 
          ( fromIntegral (sprite^.spriteOffset._1 + sprite^.spriteSize._1) / fromIntegral imageWidth
          , fromIntegral (sprite^.spriteOffset._2 + sprite^.spriteSize._2) / fromIntegral imageHeight)

      meshBl = (0, 0) 
      -- (texCoordsBl^._1 * fromIntegral imageWidth, texCoordsBl^._2 * fromIntegral imageHeight)
      meshTl = (0, meshHeight)
      meshTr = (meshWidth, meshHeight)
      meshBr = (meshWidth, 0)

      meshWidth = (texCoordsTr^._1 - texCoordsTl^._1) * fromIntegral imageWidth
      meshHeight = (texCoordsBl^._2 - texCoordsTl^._2) * fromIntegral imageHeight

-- | this is a test
-- | TODO: purge
initRenderer :: StateT RenderManager IO (SpriteRenderer, SpriteRenderUnit)
initRenderer = do 
  spriteManager <- lift newSpriteManager
  let Just spriteAtlas = spriteManager^.smAtlas.at "monsters"
      Just sprite = spriteAtlas^.saSprites.at "WolfWalk1Left"
      sprInst = newSpriteInstance sprite (50, 50) 0

  spriteRenderer <- newSpriteRenderer spriteManager
  renderUnit <- lift $ newSpriteRenderUnit spriteRenderer 100

  let sru = execState (setSpriteInstances spriteRenderer [sprInst]) renderUnit 
  lift $ uploadRenderUnit sru

  return (spriteRenderer, sru)
