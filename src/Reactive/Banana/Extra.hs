module Reactive.Banana.Extra
  ( once
  , Dynamic
  , current
  , updates
  , stepperD
  , accumD
  ) where

import Control.Monad
import Reactive.Banana

-- Bad... no GC
once :: MonadMoment m => Event a -> m (Event a)
once e = do
  b <- stepper True (False <$ e)
  pure (whenE b e)

data Dynamic a
  = Dynamic (Behavior a) (Event a)
  deriving Functor

current :: Dynamic a -> Behavior a
current (Dynamic x _) = x

updates :: Dynamic a -> Event a
updates (Dynamic _ x) = x

stepperD :: MonadMoment m => a -> Event a -> m (Dynamic a)
stepperD x xs = do
  b <- stepper x xs
  pure (Dynamic b xs)

accumD :: MonadMoment m => a -> Event (a -> a) -> m (Dynamic a)
accumD x fs = do
  (e, b) <- mapAccum x ((\f -> join (,) . f) <$> fs)
  pure (Dynamic b e)
