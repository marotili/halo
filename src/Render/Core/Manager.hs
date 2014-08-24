{-# LANGUAGE TemplateHaskell #-}
module Render.Core.Manager
    ( newRenderManager
    , newTriangleRenderer
    , TriangleRenderer
    , FontRenderer
    , Triangle(..)
    , newFontRenderer
    , renderText
    , RenderManager
    , mkTextureUnit
    )
where

import           Debug.Trace
import           Graphics.Rendering.FreeType.Internal.Face
import qualified Data.Set as Set
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import           Control.Lens
import           Control.Monad.Morph (generalize, hoist)
import           Control.Monad.State.Strict
import           Data.Binary.IEEE754
import         qualified  Data.Map as Map
import qualified Data.Vector.Storable as V
import           Data.Word

import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Ptr

import           Render.Core.Camera
import           Render.Core.Error
import           Render.Core.Render
import           Render.Core.Text
                 
import           Data.Maybe

data RenderManager = RenderManager
    { _rmUsedTextures :: Set.Set GL.TextureUnit
    } deriving (Show)

newRenderManager :: RenderManager
newRenderManager = RenderManager Set.empty


makeLenses ''RenderManager

--generalize :: Monad m => Identity a -> m a
--generalize = return . runIdentity

type Position = (Float, Float)
type Color = (Float, Float, Float, Float)

data Triangle = Triangle
    { _triangleV0 :: (Position, Color)
    , _triangleV1 :: (Position, Color)
    , _triangleV2 :: (Position, Color)
    }
makeLenses ''Triangle

-- | creates a list for the vertex array
triangleToList :: Triangle -> [Word32]
triangleToList triangle = map floatToWord
    -- | zero for padding
    [ x0, y0, r0, g0, b0, a0, 0, 0
    , x1, y1, r1, g1, b1, a1, 0, 0
    , x2, y2, r2, g2, b2, a2, 0, 0
    ]
    where
        (x0, y0) = triangle^.triangleV0._1
        (x1, y1) = triangle^.triangleV1._1
        (x2, y2) = triangle^.triangleV2._1
        (r0, g0, b0, a0) = triangle^.triangleV0._2
        (r1, g1, b1, a1) = triangle^.triangleV1._2
        (r2, g2, b2, a2) = triangle^.triangleV2._2

data TriangleRenderer = TriangleRenderer
    { _triangleData       :: [Triangle]
    , _triangleBuffer     :: GL.BufferObject
    , _triangleBufferSize :: Int
    , _triangleElements   :: GL.BufferObject
    , _triangleVAO        :: GL.VertexArrayObject
    , _triangleShader     :: GL.Program
    }
makeLenses ''TriangleRenderer

-- | return the total number of vertices inside the renderer
-- | note: equals the length of elements in triangleElements
trNumVertices :: Getter TriangleRenderer Int
trNumVertices = to (\tr -> 3*length (tr^.triangleData))

-- | Create a static renderer for triangles
newTriangleRenderer :: [Triangle] -> IO TriangleRenderer
newTriangleRenderer triangles = do
    [buffer, elementBuffer] <- GL.genObjectNames 2 :: IO [GL.BufferObject]
    [vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
    program <- setupShaders "untextured.vert" "untextured.frag"
    GL.currentProgram $= Just program
    GL.bindFragDataLocation program "color_final" $= 0
    logGL "triangle renderer setup shaders"
    _ <- uniformInfo program

    let vertexData = V.fromList $ concatMap triangleToList triangles
    let elementData = V.fromList $ map fromIntegral $ concatMap (\(i, l) -> map ((3*i) +) l) $ zip [0..length triangles - 1] (repeat [0, 1, 2]) :: V.Vector CUInt
    uploadFromVec (V.length vertexData) GL.ArrayBuffer buffer vertexData
    uploadFromVec (3*length triangles) GL.ElementArrayBuffer elementBuffer elementData
    logGL "upload data"

    GL.bindVertexArrayObject $= Just vao
    logGL "bind vao"
    GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
    logGL "get pos loc"
    GL.AttribLocation colorLoc <- GL.get $ GL.attribLocation program "myColor"
    logGL "get color loc"
    print colorLoc
    print posLoc

    GL.bindBuffer GL.ArrayBuffer $= Just buffer
    logGL "bind array buffer"
    GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 32 nullPtr
    logGL "set pos loc"
    GLRaw.glEnableVertexAttribArray posLoc
    logGL "enable pos attrib"

    GLRaw.glVertexAttribPointer colorLoc 4 GLRaw.gl_FLOAT 0 32 (plusPtr nullPtr 8)
    logGL "set color loc"
    GLRaw.glEnableVertexAttribArray colorLoc
    logGL "enable color attrib"

    print vertexData
    print elementData

    return  TriangleRenderer
        { _triangleData = triangles
        , _triangleBuffer = buffer
        , _triangleBufferSize = V.length vertexData
        , _triangleElements = elementBuffer
        , _triangleVAO = vao
        , _triangleShader = program
        }

-- | render all triangles stored in triangle renderer
-- | note: set camera first
renderTriangles :: TriangleRenderer -> Camera -> IO ()
renderTriangles renderer cam = do
    GL.currentProgram $= Just (renderer^.triangleShader)
    programSetViewProjection (renderer^.triangleShader) cam

    GL.bindVertexArrayObject $= Just (renderer^.triangleVAO)
    GL.bindBuffer GL.ElementArrayBuffer $= Just (renderer^.triangleElements)
    --GLRaw.glDrawElements GLRaw.gl_TRIANGLES (fromIntegral $ renderer^.trNumVertices) GLRaw.gl_UNSIGNED_INT nullPtr
    GLRaw.glDrawElements GLRaw.gl_TRIANGLES 3 GLRaw.gl_UNSIGNED_INT nullPtr

data TextBuffer = TextBuffer
  { _tbVertices :: V.Vector Word32
  , _tbElements :: V.Vector Word32
  }

data FontRenderer = FontRenderer
    { _frTextAtlas        :: TextAtlas
    , _frAtlasTexture     :: GL.TextureObject
    , _frAtlasTextureUnit :: Maybe GL.TextureUnit
    , _frProgram          :: GL.Program
    , _frVAO              :: GL.VertexArrayObject
    , _frTopoBuffer       :: GL.BufferObject
    , _frColorBuffer      :: GL.BufferObject
    , _frElementBuffer    :: GL.BufferObject
    , _frAtlasUniform     :: GL.BufferObject
    , _ftFace :: FT_Face
    }
makeLenses ''FontRenderer

-- | get a new texture unit
mkTextureUnit :: State RenderManager GL.TextureUnit
mkTextureUnit = do
    usedTextures <- use rmUsedTextures
    let texUnit = head $ filter (\unit -> not $ Set.member unit usedTextures) . map GL.TextureUnit $ [0..]
    rmUsedTextures %= Set.insert texUnit
    return texUnit
    
type BufferLocation = Int
type FreeSize = Int -- ^ In Bytes

data TextCache = TextCache
  { _tcBuffer :: GL.BufferObject
  , _tcTexts :: Map.Map String BufferLocation
  , _tcFreeSpace :: [(BufferLocation, FreeSize)]
  }
  
tcResize :: TextCache -> IO TextCache
tcResize = undefined
         
ret :: Getter a (IO a)
ret = to (return)
     
l = do
  t <- newTextCache
  t^.ret
         
         
newTextCache :: IO TextCache
newTextCache =
  let initialSize = 4 * 4 * 100 -- 100 chars (4 byte per char)
      freeSpace = [(0, initialSize)]
  in return TextCache
    { _tcBuffer = GL.nullBuffer
    , _tcTexts = Map.empty
    , _tcFreeSpace = freeSpace
    }

tcAddText :: String -> TextCache -> IO TextCache
tcAddText = undefined
  
-- TODO: fragmentation, referencecount

-- | initialize a new font renderer using a loaded text atlas
newFontRenderer :: TextAtlas -> StateT RenderManager IO FontRenderer
newFontRenderer textAtlas = do
    textureUnit <- hoist generalize mkTextureUnit
    lift $ do
        fm <- newFontManager
        font <- newFont fm "/home/marco/workspace/haskell/henge/games/boom/data/font.otf" 64
        face <- peek $ font^.fontFace
        (shaped, _) <- shapeLine face (newText "AVKERNimlno") 1024
        let glyphs' = map (\gl -> textAtlas^.atlasCodepointGlyphs.at (gl^.scCodepoint).to fromJust.glyphChar) shaped
        print glyphs'
        let indices = map atlasIndex glyphs'
    

        [topoBuffer, colorBuffer, elementBuffer, atlasBuffer] <- GL.genObjectNames 4 :: IO [GL.BufferObject]
        [vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
        program <- setupShaders "text.vert" "text.frag"
        GL.currentProgram $= Just program
    
        let vertexData = V.fromList $ concatMap (\(index, glyph) -> [floatToWord $ glyph^.scOffset._1, floatToWord $ glyph^.scOffset._2, fromInteger index::Word32, floatToWord 0]) $ zip indices shaped

        -- let vertexData = V.fromList $ [floatToWord 3, floatToWord 0, 0, 0] ++ [floatToWord 77, floatToWord 0, 20, 0]
        let elementData = V.fromList $ take (V.length vertexData) [(0::Word32), (1::Word32)..] :: V.Vector Word32
        let atlasData = traceShow vertexData $ atlasToStorable textAtlas
        uploadFromVec (V.length vertexData) GL.ArrayBuffer topoBuffer vertexData
        uploadFromVec (V.length elementData) GL.ElementArrayBuffer elementBuffer elementData
        uploadFromVec (V.length atlasData) GL.UniformBuffer atlasBuffer atlasData
        logGL "upload data"


        GL.bindVertexArrayObject $= Just vao
        GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
        --GL.AttribLocation myColorLoc <- GL.get $ GL.attribLocation program "myColor"
        GL.AttribLocation charIdLoc <- GL.get $ GL.attribLocation program "charId"

        GL.bindBuffer GL.ArrayBuffer $= Just topoBuffer
        GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 16 nullPtr
        GLRaw.glVertexAttribDivisor posLoc 1
        GLRaw.glEnableVertexAttribArray posLoc

        GLRaw.glVertexAttribIPointer charIdLoc 1 GLRaw.gl_INT 16 (plusPtr nullPtr 8)
        GLRaw.glVertexAttribDivisor charIdLoc 1
        GLRaw.glEnableVertexAttribArray charIdLoc

        [imageTexture] <- GL.genObjectNames 1 :: IO [GL.TextureObject]

        let image = textAtlas^.atlasImage
        uploadImage textureUnit imageTexture image

        return FontRenderer
            { _frTextAtlas = textAtlas
            , _frAtlasTexture = imageTexture
            , _frAtlasTextureUnit = Just textureUnit
            , _frProgram = program
            , _frVAO = vao
            , _frTopoBuffer = topoBuffer
            , _frColorBuffer = colorBuffer
            , _frElementBuffer = elementBuffer
            , _frAtlasUniform = atlasBuffer
            }

-- | render a text using the font renderer
renderText :: FontRenderer -> Camera -> IO ()
renderText fr cam = do
    GL.currentProgram $= Just (fr^.frProgram)
    programSetViewProjection (fr^.frProgram) cam

    charMapIndex <- GL.getUniformBlockIndex (fr^.frProgram) "CharMap"
    logGL "renderText: getUniformBlockIndex"
    GL.bindBufferBase' GL.UniformBuffer charMapIndex (fr^.frAtlasUniform)
    logGL "renderText: uniform block bind buffer base'"
    GL.uniformBlockBinding (fr^.frProgram) charMapIndex charMapIndex
    logGL "renderText: uniform block binding"

    sampler <- GL.get $ GL.uniformLocation (fr^.frProgram) "Texture0"
    logGL "renderText: uniform location"
    let Just textureUnit = fr^.frAtlasTextureUnit
    GL.uniform sampler $= textureUnit
    logGL "renderText: texture unit"

    GL.bindVertexArrayObject $= Just (fr^.frVAO)
    logGL "renderText: bindvao"
    GL.bindBuffer GL.ElementArrayBuffer $= Just (fr^.frElementBuffer)
    logGL "renderText: bindElementbuffer"
    --GLRaw.glDrawElements GLRaw.gl_TRIANGLES (fromIntegral $ renderer^.trNumVertices) GLRaw.gl_UNSIGNED_INT nullPtr
    --GLRaw.glDrawElements GLRaw.gl_TRIANGLES 12 GLRaw.gl_UNSIGNED_INT nullPtr
    GLRaw.glDrawElementsInstanced GLRaw.gl_TRIANGLES 6 GLRaw.gl_UNSIGNED_INT (plusPtr nullPtr (2*4*6)) 4
    -- GLRaw.glDrawArraysInstancedBaseInstance GLRaw.gl_TRIANGLES 1 6 1 
    --let from = 6
    -- let count = 6
    -- GLRaw.glDrawArraysInstanced GLRaw.gl_TRIANGLES from count 4
    logGL "renderText: glDrawElements"

