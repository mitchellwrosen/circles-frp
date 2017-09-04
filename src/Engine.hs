{-# language RecordWildCards #-}

-- A simple game engine abstraction.

module Engine
  ( initialize
  , run
  , ticks
  , mouse
  , leftMouseDown
  , leftMouseUp
  , rightMouseDown
  , rightMouseUp
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

data Engine = Engine
  { engineTicks  :: MomentIO (Event Float)
  , engineMouse :: MomentIO (Event (Point V2 Int32))
  , engineLeftMouseDown :: MomentIO (Event (Point V2 Int32))
  , engineLeftMouseUp :: MomentIO (Event (Point V2 Int32))
  , engineRightMouseDown :: MomentIO (Event (Point V2 Int32))
  , engineRightMouseUp :: MomentIO (Event (Point V2 Int32))
  , engineRender :: Cairo.Render () -> IO ()
  , engineLoop :: IO ()
  }

initialize :: Text -> IO Engine
initialize name = do
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

  spawnTickThread fireTick

  let engineTicks :: MomentIO (Event Float)
      engineTicks = fromAddHandler tickAddHandler

      engineMouse :: MomentIO (Event (Point V2 Int32))
      engineMouse = fromAddHandler mouseAddHandler

      engineLeftMouseDown :: MomentIO (Event (Point V2 Int32))
      engineLeftMouseDown = fromAddHandler leftMouseDownAddHandler

      engineLeftMouseUp :: MomentIO (Event (Point V2 Int32))
      engineLeftMouseUp = fromAddHandler leftMouseUpAddHandler

      engineRightMouseDown :: MomentIO (Event (Point V2 Int32))
      engineRightMouseDown = fromAddHandler rightMouseDownAddHandler

      engineRightMouseUp :: MomentIO (Event (Point V2 Int32))
      engineRightMouseUp = fromAddHandler rightMouseUpAddHandler

      engineRender :: Cairo.Render () -> IO ()
      engineRender m = do
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

      engineLoop :: IO ()
      engineLoop = do
          SDL.Event _ payload <- SDL.waitEvent
          case payload of
            -- Hard-code "q = quit" into engine, for now.
            SDL.KeyboardEvent event ->
              case SDL.keysymKeycode (SDL.keyboardEventKeysym event) of
                SDL.KeycodeQ -> pure ()
                _ -> engineLoop
            SDL.MouseMotionEvent event -> do
              fireMouse (SDL.mouseMotionEventPos event)
              engineLoop
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
              engineLoop
            _ -> engineLoop

  pure (Engine{..})

run :: Engine -> MomentIO (Event (Cairo.Render ())) -> IO ()
run engine m = do
  network <- compile $ do
    e <- m
    reactimate (engineRender engine <$> e)
  actuate network
  engineLoop engine

ticks :: Engine -> MomentIO (Event Float)
ticks = engineTicks

mouse :: Engine -> MomentIO (Event (Point V2 Int32))
mouse = engineMouse

leftMouseDown :: Engine -> MomentIO (Event (Point V2 Int32))
leftMouseDown = engineLeftMouseDown

leftMouseUp :: Engine -> MomentIO (Event (Point V2 Int32))
leftMouseUp = engineLeftMouseUp

rightMouseDown :: Engine -> MomentIO (Event (Point V2 Int32))
rightMouseDown = engineRightMouseDown

rightMouseUp :: Engine -> MomentIO (Event (Point V2 Int32))
rightMouseUp = engineRightMouseUp

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
