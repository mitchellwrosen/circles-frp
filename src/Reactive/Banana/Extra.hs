module Reactive.Banana.Extra
  ( filterPrism
  ) where

import Control.Lens
import Reactive.Banana

filterPrism :: Prism' s a -> Event s -> Event a
filterPrism p e = filterJust (preview p <$> e)
