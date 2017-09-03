module Main where

import Input
import Reactive.Banana.Extra (filterPrism)

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Data.Int (Int32)
import Data.Maybe (isJust, listToMaybe)
import Foreign.Ptr (castPtr)
import Linear (V2(V2), qd)
import Linear.Affine ((.-.))
import Reactive.Banana
import Reactive.Banana.Frameworks
import SDL
  (InputMotion(Pressed, Released), MouseButton(ButtonLeft, ButtonRight),
    Point(P), get)

import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL


radius :: Int32
radius = 20

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Circles" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  size <- get (SDL.windowSize window)
  texture <-
    SDL.createTexture renderer SDL.ARGB8888 SDL.TextureAccessStreaming size

  (inputAddHandler, fireInput) <- newAddHandler
  (tickAddHandler, fireTick) <- newAddHandler

  network <- compile $ mdo
    eInput :: Event Input <-
      fromAddHandler inputAddHandler

    eTick :: Event () <-
      fromAddHandler tickAddHandler

    let eMouse :: Event (Point V2 Int32)
        eMouse = filterPrism _InputMouse eInput

        eClick :: Event (Point V2 Int32, MouseButton, InputMotion)
        eClick = filterPrism _InputClick eInput

        eLeftClickDown :: Event (Point V2 Int32)
        eLeftClickDown = filterJust (f <$> eClick)
         where
          f = \case
            (pos, ButtonLeft, Pressed) -> Just pos
            _ -> Nothing

        eLeftClickUp :: Event (Point V2 Int32)
        eLeftClickUp = filterJust (f <$> eClick)
         where
          f = \case
            (pos, ButtonLeft, Released) -> Just pos
            _ -> Nothing

        eRightClickDown :: Event (Point V2 Int32)
        eRightClickDown = filterJust (f <$> eClick)
         where
          f = \case
            (pos, ButtonRight, Pressed) -> Just pos
            _ -> Nothing

    bCircles :: Behavior [Behavior (Point V2 Int32)] <-
      accumB [] (unions
        [ ((:) <$> observeE
            (bCircleGen eMouse eLeftClickDown eLeftClickUp <$> eNewCircle))

        , deleteIx <$>
            filterJust
              (observeE
                ((\xs p -> listToMaybe . filterIxs (p `inCircle`) <$> xs)
                  <$> bCirclesAt
                  <@> eRightClickDown))
        ])

    let -- A time-varying computation that calculates the circles that exist at
        -- that time.
        bCirclesAt :: Behavior (Moment [Point V2 Int32])
        bCirclesAt = valueB . sequenceA <$> bCircles

        eNewCircle :: Event (Point V2 Int32)
        eNewCircle = filterJust (observeE (go <$> bCircles <@> eLeftClickDown))
         where
          go :: [Behavior (Point V2 Int32)]
             -> Point V2 Int32
             -> Moment (Maybe (Point V2 Int32))
          go xs p = do
            ps <- valueB (sequenceA xs)
            if any (nearby p) ps
              then pure Nothing
              else pure (Just p)

    let -- Tag each tick with the circles that exist at that time.
        eCircles :: Event [Point V2 Int32]
        eCircles = observeE (bCirclesAt <@ eTick)

        renderCircles :: [Point V2 Int32] -> IO ()
        renderCircles xs = do
          texture_info <- SDL.queryTexture texture
          (pixels, pitch) <- SDL.lockTexture texture Nothing
          Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32
            (fromIntegral (SDL.textureWidth texture_info))
            (fromIntegral (SDL.textureHeight texture_info))
            (fromIntegral pitch)
            (\surface -> Cairo.renderWith surface $ do
              Cairo.setSourceRGB 0 0 0
              Cairo.paint
              Cairo.setSourceRGB 1 0 0
              forM_ xs $ \(P (V2 x y)) -> do
                Cairo.arc (fromIntegral x) (fromIntegral y)
                  (fromIntegral radius) 0 (2*pi)
                Cairo.fill
                Cairo.newPath)
          SDL.unlockTexture texture
          SDL.copy renderer texture Nothing Nothing
          SDL.present renderer

    reactimate (renderCircles <$> eCircles)

  actuate network

  void . forkIO . forever $ do
    fireTick ()
    threadDelay 25000 -- 40fps

  let loop =
        waitInput >>= \case
          InputKey SDL.KeycodeQ -> pure ()
          input -> do
            fireInput input
            loop

  loop

bCircleGen
  :: (MonadFix m, MonadMoment m)
  => Event (Point V2 Int32)
  -> Event (Point V2 Int32)
  -> Event (Point V2 Int32)
  -> Point V2 Int32
  -> m (Behavior (Point V2 Int32))
bCircleGen eMouse eLeftClickDown eLeftClickUp p0 = mdo
  bPos :: Behavior (Point V2 Int32) <-
    stepper p0
      (filterJust
        ((\mdrag (P mouse) -> do
          drag <- mdrag
          pure (P (mouse - drag)))
        <$> bDragging <@> eMouse))

  let eDragStart :: Event (V2 Int32)
      eDragStart =
        filterJust
          ((\pos click ->
            if click `inCircle` pos
              then Just (click .-. pos)
              else Nothing)
          <$> bPos <@> eLeftClickDown)

      eDragEnd :: Event (Point V2 Int32)
      eDragEnd = whenE (isJust <$> bDragging) eLeftClickUp

  -- When Nothing, not dragging. When Just, dragging with the mouse at
  -- the given vector from the center of this circle.
  bDragging :: Behavior (Maybe (V2 Int32)) <-
    accumB Nothing (unions
      [ const . Just <$> eDragStart
      , const Nothing <$ eDragEnd
      ])

  pure bPos

nearby :: Point V2 Int32 -> Point V2 Int32 -> Bool
nearby x y = qd x y < 4*radius*radius

inCircle :: Point V2 Int32 -> Point V2 Int32 -> Bool
inCircle p c = qd p c < radius*radius

deleteIx :: Int -> [a] -> [a]
deleteIx 0 (_:xs) = xs
deleteIx i (x:xs) = x : deleteIx (i-1) xs

-- Like 'filter', but keep only the indexes.
filterIxs :: (a -> Bool) -> [a] -> [Int]
filterIxs p = map fst . filter (\(_, x) -> p x) . zip [0..]
