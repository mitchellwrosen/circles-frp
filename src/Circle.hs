module Circle
  ( Circle(..)
  , contains
  , render
  ) where

import Data.Int (Int32)
import Linear (V2(V2), qd)
import Linear.Affine (Point(P))

import qualified Graphics.Rendering.Cairo as Cairo

data Circle = Circle
  { rgb :: (Double, Double, Double)
  , center :: Point V2 Int32
  , radius :: Int32
  }

contains :: Point V2 Int32 -> Circle -> Bool
contains point Circle{center, radius} = qd point center < radius*radius

render :: Circle -> Cairo.Render ()
render (Circle (r,g,b) (P (V2 x y)) rad) = do
  Cairo.setSourceRGB r g b
  Cairo.arc (fromIntegral x) (fromIntegral y) (fromIntegral rad) 0 (2*pi)
  Cairo.fill
  Cairo.newPath
