{-# language RecordWildCards #-}

module Main where

import Circle (Circle(Circle))
import Engine

import qualified Circle

import Control.Monad.Fix
import Data.Int (Int32)
import Data.List (delete, findIndex)
import Data.Maybe (isJust)
import Linear (V2, qd)
import Linear.Affine (Point(P), (.-.))
import Reactive.Banana
import Reactive.Banana.Frameworks
import SDL (Keycode, Keysym(..))

radius :: Int32
radius = 20

main :: IO ()
main = do
  run "Circles" $ \Input{..} -> mdo
    eChord :: Event [Keycode] <-
      accumE [] (unions
        [ (:) . keysymKeycode <$> eKeyDown
        , delete . keysymKeycode <$> eKeyUp
        ])

    -- The game state: a varying list of independently varying circles
    bCircles :: Behavior [Behavior Circle] <-
      accumB [] (unions
        [ eAddCircle
        , eDeleteCircle
        ])

    let eAddCircle :: Event ([Behavior Circle] -> [Behavior Circle])
        eAddCircle = (:) <$> observeE eNewCircle
         where
          eNewCircle :: Event (Moment (Behavior Circle))
          eNewCircle =
            bCircleGen eMouse eLeftMouseDown eLeftMouseUp <$> eNewCircleCenter

          -- An event that fires with the center of a new circle to create.
          -- Circles can only be created sufficiently far away from all other
          -- circles.
          eNewCircleCenter :: Event (Point V2 Int32)
          eNewCircleCenter =
            filterJust (observeE (go <$> bCirclesAt <@> eLeftMouseDown))
           where
            go :: Moment [Circle]
               -> Point V2 Int32
               -> Moment (Maybe (Point V2 Int32))
            go circles click = do
              circles' <- circles
              if any (nearby click) (map Circle.center circles')
                then pure Nothing
                else pure (Just click)

        eDeleteCircle :: Event ([Behavior Circle] -> [Behavior Circle])
        eDeleteCircle = deleteIx <$> filterJust (observeE eDeleteCircleAt)
         where
          eDeleteCircleAt :: Event (Moment (Maybe Int))
          eDeleteCircleAt =
            (\xs p -> findIndex (Circle.contains p) <$> xs)
              <$> bCirclesAt
              <@> eRightMouseDown

        -- A time-varying computation that calculates the circles that exist at
        -- that time.
        bCirclesAt :: Behavior (Moment [Circle])
        bCirclesAt = valueB . sequenceA <$> bCircles

    dynamicGame (fmap (mapM_ Circle.render) <$> bCirclesAt)

-- Generate a new circle behavior, given mouse events and an initial position.
bCircleGen
  :: (MonadFix m, MonadMoment m)
  => Event (Point V2 Int32)
  -> Event (Point V2 Int32)
  -> Event (Point V2 Int32)
  -> Point V2 Int32
  -> m (Behavior Circle)
bCircleGen eMouse eLeftMouseDown eLeftMouseUp p0 = mdo
  -- The center of the circle starts at the given coordinate, but tracks the
  -- mouse while it's being dragged.
  bCenter :: Behavior (Point V2 Int32) <-
    stepper p0
      (filterJust
        ((\mdrag (P mouse) -> do
          drag <- mdrag
          pure (P (mouse - drag)))
        <$> bDragging <@> eMouse))

  let -- An event that fires when this circle starts dragging. The vector that
      -- the event carries points from the center of the circle to the click.
      eDragStart :: Event (V2 Int32)
      eDragStart =
        filterJust
          ((\circle click ->
            if Circle.contains click circle
              then Just (click .-. Circle.center circle)
              else Nothing)
          <$> bCircle <@> eLeftMouseDown)

      -- An event that fires when this circle is let go.
      eDragEnd :: Event (Point V2 Int32)
      eDragEnd = whenE (isJust <$> bDragging) eLeftMouseUp

  -- Are we currently dragging, per eDragStart/eDragEnd?
  bDragging :: Behavior (Maybe (V2 Int32)) <-
    stepper Nothing
      (unionWith const (Just <$> eDragStart) (Nothing <$ eDragEnd))

  let bCircle :: Behavior Circle
      bCircle = Circle <$> pure (1,0,0) <*> bCenter <*> pure radius

  pure bCircle

nearby :: Point V2 Int32 -> Point V2 Int32 -> Bool
nearby x y = qd x y < 4*radius*radius

deleteIx :: Int -> [a] -> [a]
deleteIx 0 (_:xs) = xs
deleteIx i (x:xs) = x : deleteIx (i-1) xs
