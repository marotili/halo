{-# LANGUAGE TemplateHaskell, Rank2Types, ImpredicativeTypes #-}
module Render.Core.Text
    ( TextAtlas
    , atlasToStorable
    , atlasImage
    , newFontManager
    , deleteFontManager
    , newFont
    , newFontAtlas
    , fontFace
    , atlasCodepointGlyphs
    , glyphChar
    , atlasIndex
    , scCodepoint
    , scOffset
    , shapeLine
    , newText
    )
where

import           Data.Char
import qualified Data.Vector.Storable as V
--import Foreign.C.Types
import           Data.Binary.IEEE754
import           qualified Data.Text as T

-- | TODO: Resource management (Fonts etc.)

--import qualified Filesystem.Path as FP
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.Vector
import           Graphics.Rendering.FreeType.Internal.Bitmap
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Graphics.Rendering.FreeType.Internal.Matrix
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Foreign.Marshal.Alloc
import           Foreign.C.String
import           Foreign.Storable
import           Foreign.Ptr

import           Bindings.Harfbuzz.Hb
import           Bindings.Harfbuzz.HbFt
import           Bindings.Harfbuzz.HbShape

import           Codec.Picture

--import qualified Data.ByteString as BS
import           Data.ByteString.Internal (c2w)

import qualified Data.Map as Map
import           Control.Lens
import           Control.Monad
import           Control.Applicative
import           GHC.Word

type Coords = (Float, Float)

data Rect = Rect Coords Coords

data TextAtlas = TextAtlas
    --{ _glTexture :: GL.Texture
    { _atlasCharGlyphs :: Map.Map Char Glyph
    , _atlasCodepointGlyphs :: Map.Map Int Glyph
    , _atlasCharCoords :: Map.Map Char (Int, Int)
    , _atlasImage :: DynamicImage
    , _atlasSize :: (Int, Int)
    } 



type FontSize = Int
data Font = Font
    { _fontFace :: Ptr FT_Face
    , _fontFile :: String --FP.FilePath
    , _fontSize :: FontSize
    } deriving Show

data FontManager = FontManager
    { _fmFreetypeLib :: Ptr FT_Library
    } deriving Show

data Glyph = Glyph
    { _glyphWidth :: Int
    , _glyphHeight :: Int
    , _glyphPitch :: Int
    , _glyphData :: [Word8]
    , _glyphChar :: Char
    , _glyphAdvance :: (Int, Int)
    , _glyphTop :: Int
    , _glyphLeft :: Int
    , _glyphCodepoint :: Int
    } deriving (Show)
    
data Text = Text
  { _textString :: T.Text
  } deriving (Show)
  
newText :: String -> Text
newText = Text . T.pack
  
data ShapedChar = ShapedChar
  { _scCodepoint :: Int
  , _scOffset :: (Float, Float)
  } deriving (Show)
makeLenses ''Text 
makeLenses ''ShapedChar 

textWords :: Getter T.Text [T.Text] 
textWords = to T.words

makeLenses ''TextAtlas
makeLenses ''Glyph
makeLenses ''Font
makeLenses ''FontManager

data TextBuffer = TextBuffer
  { _tbVertices :: V.Vector Word32
  , _tbElements :: V.Vector Word32
  , _tbTexts :: ()
  } deriving (Show)

chars :: String
-- chars = map chr [0..128] -- "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
chars = "A1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

atlasIndex char = Map.fromList (zip chars [0..]) Map.! char
-- | flatten atlas for use in uniform (text.vert)
atlasToStorable :: TextAtlas -> V.Vector Word32
atlasToStorable ta = V.fromList $ map floatToWord $ concatMap getMesh chars
    where
        (atlasImageWidth, atlasImageHeight) = (fromIntegral $ ta^.atlasSize._1, fromIntegral $ ta^.atlasSize._2)
        getMesh :: Char -> [Float]
        getMesh char =
            let Just meshGlyph = ta^.atlasCharGlyphs.at char
                Just coords = ta^.atlasCharCoords.at char
                meshWidth = meshGlyph^.glyphWidth
                meshHeight = meshGlyph^.glyphHeight
            in [ fromIntegral (coords^._1) / atlasImageWidth
               , fromIntegral (coords^._2) / atlasImageHeight
               , fromIntegral meshWidth / atlasImageWidth
               , fromIntegral meshHeight / atlasImageHeight
               ]

newFontManager :: IO FontManager
newFontManager = do
    libPtr <- malloc
    _ <- ft_Init_FreeType libPtr

    return FontManager
        { _fmFreetypeLib = libPtr
        }

deleteFontManager :: FontManager -> IO ()
deleteFontManager fontManager = do
    free (fontManager^.fmFreetypeLib)
    return ()

--newFont :: FontManager -> FP.FilePath -> FontSize -> IO Font
newFont :: FontManager -> String -> FontSize -> IO Font
newFont fontManager path fntSize = do
    facePtr <- malloc
    ftLib <- peek (fontManager^.fmFreetypeLib)
    _ <- newFace path facePtr ftLib

    let hrez = 100
    let vrez = 100

    fontFace' <- peek facePtr
    _ <- ft_Set_Char_Size fontFace' 0 (fromIntegral fntSize*64) (72) 72

    let zero = 0*0x10000
    let one = 1*0x10000
    let hrez2 = round (((1.0::Float)/ (100::Float)) * 0x10000) :: FT_Fixed

    let matrix = FT_Matrix one zero zero one
    alloca $ \matPtr -> do
        poke matPtr matrix
        ft_Set_Transform fontFace' matPtr nullPtr

    return Font
        { _fontFace = facePtr
        , _fontFile = path
        , _fontSize = fntSize
        }

    where
        newFace filename fontFace' lib = withCString filename $ \cstr -> ft_New_Face lib cstr 0 fontFace'


data Direction = 
  DirLTR    
  | DirRTL 
dirToC DirLTR = c'HB_DIRECTION_LTR
dirToC DirRTL = c'HB_DIRECTION_RTL
  
data Script = 
  ScriptLatin
  | ScriptCommon
  
scriptToC ScriptLatin = c'HB_SCRIPT_LATIN
scriptToC ScriptCommon = c'HB_SCRIPT_COMMON

data HbBuffer = HbBuffer
  { hbBufferDirection :: Direction
  , hbBufferScript :: Script
  , hbBufferLanguage :: String
  , hbBufferText :: String
  , _hbBufferC :: Ptr C'hb_buffer_t
  , _hbFont :: Ptr C'hb_font_t
  , _hbFace :: FT_Face
  }
  
data HbGlyph = HbGlyph
  { _hbGlyphAdvance :: (Float, Float)
  , _hbGlyphBitmapOff :: (Int, Int)
  , _hbGlyphCodepoint :: Int
  }
 
makeLenses ''HbBuffer 
makeLenses ''HbGlyph 
           
newHbBuffer :: FT_Face -> Direction -> Script -> String -> String -> IO HbBuffer
newHbBuffer face dir script lang text = do
  hb_ft_font <- c'hb_ft_font_create face nullFunPtr
  buf <- c'hb_buffer_create
  c'hb_buffer_set_direction buf (dirToC dir)
  c'hb_buffer_set_script buf (scriptToC script)
  withCString lang $ \cstr -> do
    lang <- c'hb_language_from_string cstr (fromIntegral $ length lang)
    c'hb_buffer_set_language buf lang
  let strText = text
  withCString strText $ \cstr -> c'hb_buffer_add_utf8 buf cstr (fromIntegral $ length strText) 0 (fromIntegral $ length strText)
  return $ HbBuffer dir script lang text buf hb_ft_font face
  
shape :: HbBuffer -> IO [HbGlyph]
shape buf = do
  let cbuf = buf^.hbBufferC 
  let hb_ft_font = buf^.hbFont
  let face = buf^.hbFace
  c'hb_shape hb_ft_font cbuf nullPtr 0

  glyphCountPtr <- malloc
  hb_glyph_infos <- c'hb_buffer_get_glyph_infos cbuf glyphCountPtr
  hb_glyph_positions <- c'hb_buffer_get_glyph_positions cbuf glyphCountPtr
  
  glyphCount <- peek glyphCountPtr
  glyphs <- mapM (\ i -> do
    glyphInfo <- peekElemOff hb_glyph_infos i
    glyphPos <- peekElemOff hb_glyph_positions i
    _ <- ft_Load_Glyph face (c'hb_glyph_info_t'codepoint glyphInfo) ft_LOAD_RENDER
  
    let slotPtr = glyph face
    slot <- peek slotPtr
    let btmpPtr = bitmap slot
    let btmpTopPtr = bitmap_top slot
    let btmpLeftPtr = bitmap_left slot
    btmp <- peek btmpPtr
    btmp_top <- peek btmpTopPtr
    btmp_left <- peek btmpLeftPtr
  
    mtr <- peek (metrics slot)
    print ("metrics", mtr)
    print ("topLeft", btmp_top, btmp_left)

    return $ HbGlyph (fromIntegral (c'hb_glyph_position_t'x_advance glyphPos) / 64.0,
                fromIntegral (c'hb_glyph_position_t'y_advance glyphPos) / 64.0) 
                (fromIntegral btmp_left, fromIntegral btmp_top)
                $ fromIntegral $ c'hb_glyph_info_t'codepoint glyphInfo
    ) [0..fromIntegral glyphCount-1]
  return glyphs
   
shapeLine :: FT_Face -> Text -> Int -> IO ([ShapedChar], Text)
shapeLine face text maxWidth = do
  buf <- newHbBuffer face DirLTR ScriptLatin "en" (text^.textString.to T.unpack)
  glyphs <- shape buf  

  let (advance_x, _, shapedChars) = foldr (\glyph (advance_x, advance_y, results) ->
        (advance_x + glyph^.hbGlyphAdvance._1, advance_y + glyph^.hbGlyphAdvance._2, results ++ 
            [ShapedChar (glyph^.hbGlyphCodepoint) (advance_x + fromIntegral (glyph^.hbGlyphBitmapOff._1), advance_y + fromIntegral (glyph^.hbGlyphBitmapOff._2))])
        ) (0.0, 0.0, []) $ reverse glyphs
  
  if advance_x > fromIntegral maxWidth then
    -- remove one word
    return (shapedChars, text)
  else
    return (shapedChars, Text $ T.pack "") 
  
  
    
testHarf :: IO ()
testHarf = do
  lib <- malloc >>= \ft -> ft_Init_FreeType ft >> peek ft
  facePtr <- malloc
  withCString "/home/marco/workspace/haskell/henge/games/boom/data/font.otf" $ \fontName -> ft_New_Face lib fontName 0 facePtr
  face <- peek facePtr
  
  ft_Set_Char_Size face 0 (16*64) 72 72
  -- hb_ft_face <- c'hb_face_create face nullFunPtr
  hb_ft_font <- c'hb_ft_font_create face nullFunPtr
  
  buf <- c'hb_buffer_create
  c'hb_buffer_set_direction buf c'HB_DIRECTION_LTR
  c'hb_buffer_set_script buf c'HB_SCRIPT_COMMON
  withCString "en" $ \cstr -> do
    lang <- c'hb_language_from_string cstr 2
    c'hb_buffer_set_language buf lang
  let text = "AV"
  withCString text $ \cstr -> c'hb_buffer_add_utf8 buf cstr (fromIntegral $ length text) 0 (fromIntegral $ length text)
  c'hb_shape hb_ft_font buf nullPtr 0
  
  glyphCountPtr <- malloc 
  hb_glyph_infos <- c'hb_buffer_get_glyph_infos buf glyphCountPtr
  hb_glyph_positions <- c'hb_buffer_get_glyph_positions buf glyphCountPtr
  
  fm <- newFontManager
  f <- newFont fm "/home/marco/workspace/haskell/henge/games/boom/data/font.otf" 16
  atlas <- newFontAtlas f

  glyphCount <- peek glyphCountPtr
  mapM_ (\i -> do
    glyphInfo <- peekElemOff hb_glyph_infos i -- >>= print 
    glyph <- ft_Load_Glyph face (c'hb_glyph_info_t'codepoint glyphInfo) ft_LOAD_RENDER
    return ()
    ) [0..fromIntegral glyphCount-1]
  
  return ()
    
getGlyph :: Ptr FT_Face -> Char -> IO Glyph
getGlyph facePtr char = do
    face <- peek facePtr
    
    alloca $ (\ptrVec -> do
      aI <- ft_Get_Char_Index face (fromIntegral . ord $ 'A')
      vI <- ft_Get_Char_Index face (fromIntegral . ord $ 'V')
      vZ <- ft_Get_Char_Index face (fromIntegral . ord $ 'Z')
      let FT_Kerning_Mode m = ft_KERNING_DEFAULT
      err <- ft_Get_Kerning face aI vI (fromIntegral m) ptrVec
      err2 <- ft_Get_Kerning face vI vZ (fromIntegral m) ptrVec
      return ()
      )

    glyphIndex <- ft_Get_Char_Index face (fromIntegral . ord $ char)
    ft_Load_Glyph face glyphIndex ft_LOAD_RENDER
    -- glyphSlot <- peek $ glyph face
    -- ft_Render_Glyph glyphSlot ft_RENDER_MODE_NORMAL
    let slotPtr = glyph face
    slot <- peek slotPtr
    let bitmapPtr = bitmap slot 
    bitmap <- peek bitmapPtr
    imgData <- mapM (\(x, y) -> do
      c8 <- peekElemOff (buffer bitmap) (y*(fromIntegral . pitch $ bitmap) + x)
      return $ c2w . castCCharToChar $ c8    
      ) [(x, y) | y <- [0..(fromIntegral $ rows bitmap-1)], x <- [0..(fromIntegral $ width bitmap - 1)]]
    
    top <- peek (bitmap_top slot)
    left <- peek (bitmap_left slot)
    FT_Vector x y <- peek (advance slot)
        
    let g = Glyph
            { _glyphWidth = fromIntegral (width bitmap)
            , _glyphHeight = fromIntegral (rows bitmap)
            , _glyphPitch = fromIntegral (pitch bitmap)
            , _glyphData = imgData
            , _glyphChar = char
            , _glyphAdvance = (fromIntegral x, fromIntegral y)
            , _glyphTop = fromIntegral top
            , _glyphLeft = fromIntegral left
            , _glyphCodepoint = fromIntegral glyphIndex
            }
    return g


buildAtlas :: Ptr FT_Face -> String -> IO TextAtlas
buildAtlas facePtr fullString = do

    bitmapList <- mapM (getGlyph facePtr) fullString

    -- simple atlas
    -- TODO: binpacking
    let maxWidth = maximum $ map (^.glyphWidth) bitmapList
    let maxHeight = maximum $ map (^.glyphHeight) bitmapList

    let defaultAtlasSize = 1024
    let bitmapsPerRow = defaultAtlasSize `div` maxWidth
    let bitmapsPerColumn = defaultAtlasSize `div` maxHeight

    let offsets = take (length fullString) [(x*maxWidth, y*maxHeight) | y <- [0..bitmapsPerColumn-1], x <- [0..bitmapsPerRow-1]]

    let imgData = foldr (\(bitmap', offset) pointMap ->
            foldr (\(x, y) pointMap' -> if x - offset^._1 < (bitmap'^.glyphPitch) && y - offset^._2 < (bitmap'^.glyphHeight) then
                    Map.insert (x, y) ((bitmap'^.glyphData)!!((y - offset^._2)*fromIntegral (bitmap'^.glyphPitch) + (x - offset^._1))) pointMap'
                else
                    Map.insert (x, y) 0 pointMap'
                    ) pointMap [(x, y) | x <- [offset^._1..(offset^._1 + maxWidth - 1)], y <- [offset^._2..(offset^._2 + maxHeight - 1)]]
            ) Map.empty (zip bitmapList offsets)

    let img = generateImage (\x y -> if Map.member (x,y) imgData then imgData Map.! (x,y) else 0) defaultAtlasSize defaultAtlasSize

    let newAtlas = foldr (\(glyph', offset) atlas ->
            (atlas & atlasCharGlyphs %~ Map.insert (glyph'^.glyphChar) glyph'
                   & atlasCodepointGlyphs %~ Map.insert (glyph'^.glyphCodepoint) glyph'
                   & atlasCharCoords %~ Map.insert (glyph'^.glyphChar) offset)
            ) (TextAtlas Map.empty Map.empty Map.empty (ImageY8 img) (defaultAtlasSize, defaultAtlasSize)) (zip bitmapList offsets)

    saveBmpImage ("test" ++ show (0 :: Int) ++ ".bmp") (ImageY8 img)
    return newAtlas
newFontAtlas :: Font -> IO TextAtlas
newFontAtlas font = do
    let facePtr = font^.fontFace

    let fullString = chars
    return ()

--    hb_lang <- withCString "en" $ \lang -> c'hb_language_from_string lang 3

    -- hbBuf <- c'hb_buffer_create
    -- c'hb_buffer_set_direction hbBuf c'HB_DIRECTION_LTR
    -- c'hb_buffer_set_script hbBuf c'HB_SCRIPT_COMMON
    -- c'hb_buffer_set_language hbBuf hb_lang
    -- withCString fullString $ \text -> c'hb_buffer_add_utf8 hbBuf text (fromIntegral . length $ fullString) 0 (fromIntegral . length $ fullString)
    -- hb_font <- peek facePtr >>= \font' -> c'hb_ft_font_create font' nullFunPtr
    -- c'hb_shape hb_font hbBuf nullPtr 0

    buildAtlas facePtr fullString
    
