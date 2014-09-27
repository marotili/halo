{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
module Render.Core
(
  getSprite
, addSprite
, newSpriteRenderUnit
, getSpriteRenderUnit
, newCamera
, setCamera
, getCamera
, modifyCamera

, Renderer
, render
, runRenderControl
, newRenderer

, setPosition
, setRotation
, move
, rotate
, SpriteInstanceId(..)
, RenderControl
, newViewport
, setViewport
, getRenderControlResult
)
where

import           Debug.Trace
import           Render.Core.Camera hiding (setViewport)
import qualified Render.Core.Camera as C
import           Render.Core.Manager
import           Render.Core.Error
import           Render.Init

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import           Render.Init (withWindow)

import           Render.SpriteManager hiding (Sprite, newSpriteRenderUnit)
import qualified Render.SpriteManager as SM

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set

import qualified Data.Binary as B
import           Data.Monoid
                 
import           HSys hiding (move, rotate)

type SpriteRenderUnitName = String
type CameraName = String
type ViewportName = String
type AtlasName = String
type SpriteName = String
newtype SpriteInstanceId = SpriteInstanceId Int deriving (Show, Eq, Ord)

-- instance Monoid (Float, Float) where
--     mempty = (0, 0)
--     mappend (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type SpriteInstanceName = String
data SpriteRenderUnitInfo = SpriteRenderUnitInfo { _ruSpriteInstances  :: !(Map.Map SpriteInstanceId SpriteInstance)
                                                 , _ruSpriteRenderUnit :: !SpriteRenderUnit
                                                 , _ruNeedsUpdate      :: !Bool
                                                 , _ruViewport         :: !(Maybe ViewportName)
                                                 , _ruCamera           :: !(Maybe CameraName)
                                                 } deriving (Show)

newSpriteRenderUnitInfo :: SpriteRenderUnit -> SpriteRenderUnitInfo
newSpriteRenderUnitInfo renderUnit = SpriteRenderUnitInfo
  { _ruSpriteInstances = Map.empty
  , _ruSpriteRenderUnit = renderUnit
  , _ruNeedsUpdate = False
  , _ruViewport = Nothing
  , _ruCamera = Nothing
  }

data Renderer = Renderer
     { _rCameras           :: !(Map.Map CameraName Camera)
     , _rSpriteManager     :: !SpriteManager
     , _rSpriteRenderer    :: !SpriteRenderer
     , _rSpriteRenderUnits :: !(Map.Map SpriteRenderUnitName SpriteRenderUnitInfo)
     , _rViewports         :: !(Map.Map ViewportName Viewport)
     , _rWindow            :: !GLFW.Window
     , _rNextSpriteInstanceId :: !SpriteInstanceId
     , _rSpriteInstanceRu :: !(Map.Map SpriteInstanceId SpriteRenderUnitName)
     } deriving (Show)

makeLenses ''SpriteRenderUnitInfo
makeLenses ''Renderer

mkSpriteInstanceId :: (Monad m) => StateT Renderer m SpriteInstanceId
mkSpriteInstanceId = do
  newId <- use $ rNextSpriteInstanceId
  rNextSpriteInstanceId %= \(SpriteInstanceId id) -> SpriteInstanceId (id + 1) 
  return newId

newtype CameraId s = CameraId CameraName deriving (Eq, Ord)
data SpriteId s = SpriteId !AtlasName !SpriteName deriving (Eq, Ord, Show)
newtype SpriteRenderUnitId s = SpriteRenderUnitId SpriteRenderUnitName deriving (Show)
newtype ViewportId s = ViewportId ViewportName deriving (Show)

newRenderer :: GLFW.Window -> StateT RenderManager IO Renderer
newRenderer win = do
  spriteManager <- lift newSpriteManager
  spriteRenderer <- newSpriteRenderer spriteManager
  return Renderer { _rCameras = Map.empty
                  , _rSpriteManager = spriteManager
                  , _rSpriteRenderer = spriteRenderer
                  , _rSpriteRenderUnits = Map.empty
                  , _rViewports = Map.empty
                  , _rWindow = win
                  , _rNextSpriteInstanceId = SpriteInstanceId 1
                  , _rSpriteInstanceRu = Map.empty
                  }

render :: Renderer -> IO ()
render rdr = do
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  logGL "blendFunc"
  GL.blend $= GL.Enabled
  logGL "blend"

  clearWindow (rdr^.rWindow)
  let srus = Map.toList (rdr^.rSpriteRenderUnits)

  mapM_ (\(sruName, sru) -> do
           let sru' = execState (setSpriteInstances (rdr^.rSpriteRenderer) $
                      (Map.elems $ sru^.ruSpriteInstances)
                      ) (sru^.ruSpriteRenderUnit)
           uploadRenderUnit sru'

           let Just cam = case sru^.ruCamera of
                            Just camName -> rdr^.rCameras.at camName
                            Nothing -> error $ "Could not find camera for" ++ show sruName

           case (sru^.ruViewport) of
             Just vpName -> let Just vp = rdr^.rViewports.at vpName in C.setViewport vp
             Nothing -> return ()
           renderSprites (rdr^.rSpriteRenderer) sru' cam
        ) srus
  -- GLFW.swapBuffers (rdr^.rWindow)
  logGL "swapBuffers"
  
data RenderCommands next =
     forall s. NewCamera !CameraName !(Float, Float) (CameraId s -> next)
   | forall s. GetCamera !CameraName (CameraId s -> next)
   | forall s. ModCamera !(CameraId s) !(Camera -> Camera) next
   
   | forall s. NewSpriteRenderUnit !(SpriteRenderUnitName) (SpriteRenderUnitId s -> next)
   | forall s. GetSpriteRenderUnit !(SpriteRenderUnitName) (SpriteRenderUnitId s -> next)
   
   | forall s. GetSprite !AtlasName !SpriteName (SpriteId s -> next)
   
   | forall s s2. AddSprite !(SpriteRenderUnitId s) !(SpriteId s2) !(Float, Float) !Float (SpriteInstanceId -> next)
   | ModSpriteInstance !(SpriteInstanceId) (SpriteInstance -> SpriteInstance) next

   | forall s. NewViewport !ViewportName !Viewport (ViewportId s -> next)

   | forall s s2. SetViewport !(SpriteRenderUnitId s2) !(ViewportId s) !next
   | forall s s2. SetCamera !(SpriteRenderUnitId s) !(CameraId s2) !next

-- data RenderCommands next =
--      forall s s2. SetCamera !(SpriteRenderUnitId s) !(CameraId s2) !next
--    | GetCamera !CameraName !next
--    | NewCamera !CameraName !Float !Float !next
--    | forall s. ModifyCamera !(CameraId s) !(Camera -> Camera) !next
--    -- | GetSprite AtlasName SpriteName next
--    | NewSpriteRenderUnit !SpriteRenderUnitName !next
--    | GetSprite !AtlasName !SpriteName !next
--    | forall s s2. AddSprite !(SpriteRenderUnitId s) !(SpriteId s2) !SpriteInstanceName !(Float, Float) !Float !next
--    | forall s. ModifySpriteInstance !(SpriteRenderUnitId s) !SpriteInstanceName !(SpriteInstance -> SpriteInstance) !next
--    | forall s. NewViewport !ViewportName !Int !Int !Int !Int !next
--    | forall s s2. SetViewport !(ViewportId s) !(SpriteRenderUnitId s2) !next

-- instance Show n => Show (RenderCommands n) where
--   show (SetCamera (SpriteRenderUnitId ruName) (CameraId camName) n) = "SetCamera " ++ show ruName ++ " " ++ show camName ++ "\n" ++ show n
--   show (GetCamera camName n) = "GetCamera " ++ show camName ++ "\n" ++ show n
--   show _ = "RenderCommands"

instance Functor (RenderCommands) where
  fmap f (NewCamera cam size g) = NewCamera cam size (f . g)         
  fmap f (GetCamera cam g) = GetCamera cam (f . g)
  fmap f (ModCamera camId mod n) = ModCamera camId mod (f n)
  fmap f (NewSpriteRenderUnit ru g) = NewSpriteRenderUnit ru (f . g)
  fmap f (GetSpriteRenderUnit ru g) = GetSpriteRenderUnit ru (f . g)
  fmap f (GetSprite atlas sprite g) = GetSprite atlas sprite (f . g)
  fmap f (AddSprite ruId spriteId pos rot g) = AddSprite ruId spriteId pos rot (f . g)
  fmap f (ModSpriteInstance siId mod n) = ModSpriteInstance siId mod (f n)
  fmap f (NewViewport vpName vp g) = NewViewport vpName vp (f . g)
  fmap f (SetViewport ruId vpId n) = SetViewport ruId vpId (f n)
  fmap f (SetCamera ruId camId n) = SetCamera ruId camId (f n)
  -- fmap f (SetCamera ru c n) = SetCamera ru c (f n)
  -- fmap f (GetCamera name n) = GetCamera name (f n)
  -- fmap f (NewCamera s a b n) = NewCamera s a b (f n)
  -- fmap f (ModifyCamera cId g n) = ModifyCamera cId g (f n)
  -- fmap f (NewSpriteRenderUnit name n) = NewSpriteRenderUnit name (f n)
  -- fmap f (GetSprite aName sName n) = GetSprite aName sName (f n)
  -- fmap f (AddSprite ru s a b c n) = AddSprite ru s a b c (f n)
  -- fmap f (ModifySpriteInstance ruId name g n) = ModifySpriteInstance ruId name g (f n)
  -- fmap f (NewViewport name x y w h n) = NewViewport name x y w h (f n)
  -- fmap f (SetViewport vpName ruName n) = SetViewport vpName ruName (f n)

instance B.Binary (SpriteRenderUnitId s) where
    put (SpriteRenderUnitId name) = B.put name
    get = liftM SpriteRenderUnitId B.get

instance B.Binary (CameraId s) where
    put (CameraId name) = B.put name
    get = liftM CameraId B.get

instance B.Binary (SpriteId s) where
    put (SpriteId aName sName) = B.put aName >> B.put sName
    get = liftM2 SpriteId B.get B.get

liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6
       ; return (f x1 x2 x3 x4 x5 x6)
       }

-- instance B.Binary a => B.Binary (Free RenderCommands a) where
--     put (Free (SetCamera ru c n)) = sequence_ [B.put (1 :: Int), B.put ru, B.put c, B.put n]
--     put (Free (GetCamera name n)) = sequence_ [B.put (2 :: Int), B.put name, B.put n]
--     put (Pure a) = sequence_ [B.put (3 :: Int), B.put a]
--     put (Free (NewSpriteRenderUnit ruName n)) = sequence_ [B.put (4 :: Int), B.put ruName, B.put n]
--     put (Free (NewCamera name w h n)) = sequence_ [B.put (5 :: Int), B.put name, B.put w, B.put h, B.put n]
--     put (Free (GetSprite aName sName n)) = sequence_ [B.put (6 :: Int), B.put aName, B.put sName, B.put n]
--     put (Free (AddSprite ru s a b c n)) = sequence_ [B.put (7 :: Int), B.put ru, B.put s, B.put a, B.put b, B.put c, B.put n]

--     get = do
--       constrId <- B.get :: B.Get Int
--       case constrId of
--         1 -> Free <$> liftM3 SetCamera B.get B.get B.get
--         2 -> Free <$> liftM2 GetCamera B.get B.get
--         3 -> liftM Pure B.get
--         4 -> Free <$> liftM2 NewSpriteRenderUnit B.get B.get
--         5 -> Free <$> liftM4 NewCamera B.get B.get B.get B.get
--         6 -> Free <$> liftM3 GetSprite B.get B.get B.get
--         7 -> Free <$> liftM6 AddSprite B.get B.get B.get B.get B.get B.get
--         _ -> error "Not expected"

type RenderControl = Free RenderCommands -- :: * -> *

getSprite :: AtlasName -> SpriteName -> RenderControl (SpriteId s)
getSprite atlasName spriteName = liftF (GetSprite atlasName spriteName id)

addSprite :: SpriteRenderUnitId s1 -> SpriteId s2 -> (Float, Float) -> Float -> RenderControl (SpriteInstanceId)
addSprite sru sprite pos rot = liftF (AddSprite sru sprite pos rot id)

newSpriteRenderUnit :: SpriteRenderUnitName -> RenderControl (SpriteRenderUnitId s)
newSpriteRenderUnit ruName = liftF (NewSpriteRenderUnit ruName id)

-- | TODO: fixme (we need to create a rendercommand for this
getSpriteRenderUnit :: SpriteRenderUnitName -> RenderControl (SpriteRenderUnitId s)
getSpriteRenderUnit ruName = liftF (GetSpriteRenderUnit ruName id)

newCamera :: String -> (Float, Float) -> RenderControl (CameraId s)
newCamera cameraName size = liftF (NewCamera cameraName size id)

setCamera :: SpriteRenderUnitId s1 -> CameraId s2 -> RenderControl ()
setCamera sruId cam = liftF (SetCamera sruId cam ())

getCamera :: CameraName -> RenderControl (CameraId s)
getCamera camName = liftF (GetCamera camName id)

modifyCamera :: CameraId s -> (Camera -> Camera) -> RenderControl ()
modifyCamera cId modifyFunc = liftF (ModCamera cId modifyFunc ())

modifySpriteInstance :: SpriteInstanceId -> (SpriteInstance -> SpriteInstance) -> RenderControl ()
modifySpriteInstance name f = liftF (ModSpriteInstance name f ())

setRotation :: SpriteInstanceId -> Float -> RenderControl ()
setRotation name rot = liftF (ModSpriteInstance name (\si -> si & siRotation .~ rot) ())

setPosition :: SpriteInstanceId -> (Float, Float) -> RenderControl ()
setPosition name pos = Free (ModSpriteInstance name (\si -> si & siPosition .~ pos) (Pure ()))

move name posDelta = Free (ModSpriteInstance name (\si -> si & siPosition %~ (posDelta `mappend`)) (Pure ()))
rotate name rotDelta = Free (ModSpriteInstance name (\si -> si & siRotation %~ (rotDelta `mappend`)) (Pure ()))

newViewport :: ViewportName -> Viewport -> RenderControl (ViewportId s)
newViewport name vp = liftF (NewViewport name vp id)

setViewport :: SpriteRenderUnitId s2 -> ViewportId s -> RenderControl ()
setViewport ruId vpId = liftF (SetViewport ruId vpId ())

data NameContainer = NameContainer { _cameraNames :: !(Set.Set CameraName)
                             , _sruNames          :: !(Set.Set SpriteRenderUnitName)
                             , _atlasNames        :: !(Set.Set AtlasName)
                             , _spriteNames       :: !(Set.Set SpriteName)
                             } deriving (Show)
emptyNames :: NameContainer
emptyNames = NameContainer { _cameraNames = Set.empty
                          , _sruNames = Set.empty
                          , _atlasNames = Set.empty
                          , _spriteNames = Set.empty
                          }

makeLenses ''NameContainer

data Resources = Resources { _resourceSprites :: !(Map.Map (AtlasName, SpriteName) SM.Sprite)
                           } deriving (Show)

makeLenses ''Resources
           
getRenderControlResult :: Free RenderCommands a -> StateT Renderer Identity a 
getRenderControlResult rc = run rc (Resources Map.empty)
  where run rc res = do (resNew, rcNext) <- (runRenderControl' rc res)
                        case rcNext of Pure a -> return a
                                       _ -> run rcNext resNew
         
runRenderControl :: Free RenderCommands a -> StateT Renderer IO a
runRenderControl free = do
  --let newFree = removeMissing renderer emptyNames free
  runRenderControlIO free (Resources Map.empty)

-- | remove all actions involving resources that do not exist
-- | TODO: implement
-- removeMissing :: Renderer -> NameContainer -> Free RenderCommands a -> Free RenderCommands b
-- removeMissing renderer newNames (Free n@(SetCamera (SpriteRenderUnitId ruName) (CameraId camName) next))
--     | (Set.member ruName (newNames^.sruNames) || Map.member ruName (renderer^.rSpriteRenderUnits))
--         && (Set.member camName (newNames^.cameraNames) || Map.member camName (renderer^.rCameras)) =
--         Free (fmap (removeMissing renderer newNames) n)
--     | otherwise = removeMissing renderer newNames next

-- removeMissing renderer newNames (Free n@(NewCamera camName _ _ _)) =
--     Free (fmap (removeMissing renderer (newNames & cameraNames %~ Set.insert camName)) n)

-- removeMissing renderer newNames (Free n) =
--     Free $ fmap (removeMissing renderer newNames) n
       
runRenderControlIO :: Free RenderCommands a -> Resources -> StateT Renderer IO a
runRenderControlIO (Free (NewSpriteRenderUnit ruName f)) res = do
  ruExists <- uses rSpriteRenderUnits (Map.member ruName)
  if ruExists
     then error $ "Render unit " ++ ruName ++ " arleady exists."
     else do
       spriteRenderer <- use rSpriteRenderer
       newRU <- lift $ SM.newSpriteRenderUnit spriteRenderer 100
       rSpriteRenderUnits %= Map.insert ruName (newSpriteRenderUnitInfo newRU)
       runRenderControlIO (f $ SpriteRenderUnitId ruName) res
  
runRenderControlIO f res = do
  (newRes, rcNext) <- runRenderControl' f res
  case rcNext of Pure a -> return a
                 _ -> do runRenderControlIO rcNext newRes
        

-- | run all actions (all names should be valid)
runRenderControl' :: (Monad m) => Free RenderCommands a -> Resources -> (StateT Renderer m (Resources, Free RenderCommands a))
runRenderControl' (Free (NewCamera camName (viewportWidth, viewportHeight) f)) res =
  do
    camExists <- uses rCameras (Map.member camName)
    if camExists
      then error $ "Camera " ++ camName ++ " already exists."
      else do rCameras %= Map.insert camName (newDefaultCamera viewportWidth viewportHeight)
              return (res, f $ CameraId camName)

-- | TODO: maybe missing is not needed
-- we test anyway on access. maybe we need it for debugging purposes
runRenderControl' (Free (GetCamera camName f)) res = do
  mCam <- use $ rCameras . at camName
  case mCam of 
    Just cam -> return (res, f $ CameraId camName)
    Nothing -> error "Camera not found"

runRenderControl' (Free (NewSpriteRenderUnit ruName f)) res = do
  ruExists <- uses rSpriteRenderUnits (Map.member ruName)
  if ruExists
     then error $ "Render unit " ++ ruName ++ " arleady exists."
     else do
       spriteRenderer <- use rSpriteRenderer
       -- rSpriteRenderUnits %= Map.insert ruName (newSpriteRenderUnitInfo newRU)
       return (res, f $ SpriteRenderUnitId ruName)
  
runRenderControl' (Free (GetSpriteRenderUnit ruName f)) res = do
  mRu <- use $ rSpriteRenderUnits.at ruName
  case mRu of
    Nothing -> error ("No render unit: " ++ show ruName)
    Just ru -> do
      return (res, f $ SpriteRenderUnitId ruName)

-- we checked in a pass before if the sprite does exist
runRenderControl' (Free (GetSprite atlasName spriteName f)) res = do
  sm <- use rSpriteManager
  let Just spriteAtlas = sm^.smAtlas.at atlasName
  let Just sprite = spriteAtlas^.saSprites.at spriteName
  return (res & resourceSprites %~ Map.insert (atlasName, spriteName) sprite, f $ SpriteId atlasName spriteName)

runRenderControl' (Free (AddSprite (SpriteRenderUnitId ruName) (SpriteId atlasName spriteName) pos rot f)) res = do
  let Just sprite = res^.resourceSprites.at (atlasName, spriteName)
  spriteInstanceId <- mkSpriteInstanceId
  rSpriteInstanceRu . at spriteInstanceId .= Just ruName
  rSpriteRenderUnits.at ruName._Just.ruSpriteInstances.at spriteInstanceId .= (Just $ newSpriteInstance sprite pos rot)
  return (res, f $ spriteInstanceId)

runRenderControl' (Free (ModSpriteInstance siId mod n)) res = do
  Just ruName <- use $ rSpriteInstanceRu . at siId
  rSpriteRenderUnits.at ruName._Just.ruSpriteInstances.at siId %= Just . mod . fromJust
  return (res, n)

runRenderControl' (Free (SetCamera (SpriteRenderUnitId ruName) (CameraId cameraName) n)) res = do
  rSpriteRenderUnits.at ruName._Just.ruCamera .= (Just cameraName)
  return (res, n)

runRenderControl' (Free (ModCamera (CameraId camName) modifyFunc n)) res = do
  rCameras . at camName ._Just %= modifyFunc
  return (res, n)

-- runRenderControl' (Free (ModifySpriteInstance (SpriteRenderUnitId ruName) name modifyFunc n)) res = do
--   rSpriteRenderUnits.at ruName._Just.ruSpriteInstances.at name._Just %= modifyFunc
--   runRenderControl' n res

runRenderControl' (Free (NewViewport name vp f)) res = do
  rViewports.at name .= (Just $ vp)
  return (res, f $ ViewportId name)


runRenderControl' (Free (SetViewport (SpriteRenderUnitId ruName) (ViewportId vpName) n)) res = 
  do
    rSpriteRenderUnits.at ruName._Just.ruViewport .= Just vpName
    return (res, n)
  
runRenderControl' (Pure a) res = return (res, Pure a)

-- runRenderControl' (Pure a) res = return a

-- mainTest :: IO ()
-- mainTest = withWindow 100 100 "Test" $ \ _ -> do
--   renderer <- evalStateT (newRenderer) (newRenderManager)
--   renderer' <- execStateT (runRenderControl test) renderer

--   return ()
--   print renderer'

-- binTest = withWindow 100 100 "Test" $ \_ -> do
--   let enc = B.encode binaryTest
--   let decodedTest = B.decode enc :: RenderControl ()
--   renderer <- evalStateT (newRenderer) (newRenderManager)
--   renderer' <- execStateT (runRenderControl decodedTest) renderer

--   return ()
--   print renderer'


-- do
--   mCam <- use $ rCameras . at camName
--   return $ case mCam of
--              Nothing -> Nothing
--              Just _ -> Just (CameraId camName)


-- test :: RenderControl ()
-- test = do
--      cam <- newCamera "MainCam" 1024 1024
--      ru <- newSpriteRenderUnit "MainUnit"
--      spr <- getSprite "monsters" "WolfWalk1Left"
--      addSprite ru spr "testSprite" (100, 100) 3.14
--      setCamera ru cam
--      return ()

-- binaryTest = do
--   _ <- newCamera "MainCam" 1024 1024
--   cam <- getCamera "MainCam"
--   ru <- newSpriteRenderUnit "MainUnit"
--   setCamera cam ru
--   return ()

-- runTest :: Free RenderCommands ()
-- runTest = evalStateT (test) (Renderer Map.empty Nothing Nothing Map.empty)

-- printStuff :: Free RenderCommands n -> IO ()
-- printStuff (Free (NewCamera name width height n)) = do
--   print ("New camera: " ++ show name ++ "\n")
--   printStuff n
-- printStuff (Free (SetCamera _ (CameraId name) n)) = do
--   print ("Set camera: " ++ show name ++ "\n")
--   printStuff n
-- printStuff (Pure _) = do
--   print "\n"
