{-# language RecordWildCards #-}

-- A simple game engine abstraction.

module Engine
  ( Input(..)
  , GameLogic
  , staticGame
  , dynamicGame
  , dynamicGameIO
  , run
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Int (Int32)
import Data.Text (Text)
import Data.StateVar (get)
import Foreign.Ptr (castPtr)
import Linear (V2(V2))
import Linear.Affine (Point)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL

data Input = Input
  { eTick :: Event Float
  , eMouse :: Event (Point V2 Int32)
  , eLeftMouseDown :: Event (Point V2 Int32)
  , eLeftMouseUp :: Event (Point V2 Int32)
  , eRightMouseDown :: Event (Point V2 Int32)
  , eRightMouseUp :: Event (Point V2 Int32)
  , eKeyDown :: Event SDL.Keysym
  , eKeyUp :: Event SDL.Keysym
  }

data GameLogic
  = Logic1 (Behavior (Cairo.Render ()))
  | Logic2 (Behavior (Moment (Cairo.Render ())))
  | Logic3 (Behavior (MomentIO (Cairo.Render ())))

staticGame :: Behavior (Cairo.Render ()) -> MomentIO GameLogic
staticGame = pure . Logic1

dynamicGame :: Behavior (Moment (Cairo.Render ())) -> MomentIO GameLogic
dynamicGame = pure . Logic2

dynamicGameIO :: Behavior (MomentIO (Cairo.Render ())) -> MomentIO GameLogic
dynamicGameIO = pure . Logic3

-- | Run a game until 'q' is pressed.
run
  :: Text -- ^ Window title
  -> (Input -> MomentIO GameLogic) -- ^ Game logic
  -> IO ()
run name bLogicGen = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow name SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  size@(V2 w h) <- get (SDL.windowSize window)
  texture <-
    SDL.createTexture renderer SDL.ARGB8888 SDL.TextureAccessStreaming size

  (tickAddHandler, fireTick) <- newAddHandler
  (mouseAddHandler, fireMouse) <- newAddHandler
  (leftMouseDownAddHandler, fireLeftMouseDown) <- newAddHandler
  (leftMouseUpAddHandler, fireLeftMouseUp) <- newAddHandler
  (rightMouseDownAddHandler, fireRightMouseDown) <- newAddHandler
  (rightMouseUpAddHandler, fireRightMouseUp) <- newAddHandler
  (keyDownAddHandler, fireKeyDown) <- newAddHandler
  (keyUpAddHandler, fireKeyUp) <- newAddHandler

  spawnTickThread fireTick

  network <- compile $ do
    eTick :: Event Float <-
      fromAddHandler tickAddHandler

    eMouse :: Event (Point V2 Int32) <-
      fromAddHandler mouseAddHandler

    eLeftMouseDown :: Event (Point V2 Int32) <-
      fromAddHandler leftMouseDownAddHandler

    eLeftMouseUp :: Event (Point V2 Int32) <-
      fromAddHandler leftMouseUpAddHandler

    eRightMouseDown :: Event (Point V2 Int32) <-
      fromAddHandler rightMouseDownAddHandler

    eRightMouseUp :: Event (Point V2 Int32) <-
      fromAddHandler rightMouseUpAddHandler

    eKeyDown :: Event SDL.Keysym <-
      fromAddHandler keyDownAddHandler

    eKeyUp :: Event SDL.Keysym <-
      fromAddHandler keyUpAddHandler

    bLogic :: GameLogic <-
      bLogicGen Input{..}

    let render :: Cairo.Render () -> IO ()
        render m = do
          (pixels, pitch) <- SDL.lockTexture texture Nothing
          Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32
            (fromIntegral w) (fromIntegral h) (fromIntegral pitch)
            (\surface -> Cairo.renderWith surface $ do
              Cairo.setSourceRGB 0 0 0
              Cairo.paint
              m)
          SDL.unlockTexture texture
          SDL.copy renderer texture Nothing Nothing
          SDL.present renderer

    case bLogic of
      Logic1 bRender -> reactimate (render <$> bRender <@ eTick)
      Logic2 bRender -> reactimate (render <$> observeE (bRender <@ eTick))
      Logic3 bRender -> do
        eRender <- execute (bRender <@ eTick)
        reactimate (render <$> eRender)

  actuate network

  let loop :: IO ()
      loop = do
        SDL.Event _ payload <- SDL.waitEvent
        case payload of
          SDL.KeyboardEvent (SDL.KeyboardEventData _ motion False sym) ->
            -- Hard-code "q = quit" into engine, for now.
            case SDL.keysymKeycode sym of
              SDL.KeycodeQ -> pure ()
              _ -> do
                case motion of
                  SDL.Pressed -> fireKeyDown sym
                  SDL.Released -> fireKeyUp sym
                loop
          SDL.MouseMotionEvent event -> do
            fireMouse (SDL.mouseMotionEventPos event)
            loop
          SDL.MouseButtonEvent event -> do
            let pos = SDL.mouseButtonEventPos event
                button = SDL.mouseButtonEventButton event
                motion = SDL.mouseButtonEventMotion event
            case (button, motion) of
              (SDL.ButtonLeft, SDL.Pressed) -> fireLeftMouseDown pos
              (SDL.ButtonLeft, SDL.Released) -> fireLeftMouseUp pos
              (SDL.ButtonRight, SDL.Pressed) -> fireRightMouseDown pos
              (SDL.ButtonRight, SDL.Released) -> fireRightMouseUp pos
              _ -> pure ()
            loop
          _ -> loop

  loop

spawnTickThread :: (Float -> IO ()) -> IO ()
spawnTickThread fireTick =
  void . forkIO $ do
    let go :: Float -> IO ()
        go delta = do
          before <- getTime Monotonic
          fireTick delta
          after <- getTime Monotonic
          go (fromIntegral (toNanoSecs (after - before)) / 1e9)
    go (1/30)
