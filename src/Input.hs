{-# language TemplateHaskell #-}

module Input
  ( Input(..)
  , _InputMouse
  , _InputClick
  , _InputKey
  , waitInput
  ) where

import Control.Lens.TH (makePrisms)
import Data.Int (Int32)
import Linear (V2)
import Linear.Affine (Point)

import SDL

data Input
  = InputMouse (Point V2 Int32)
  | InputClick (Point V2 Int32) MouseButton InputMotion
  | InputKey Keycode
  deriving (Show)

-- _InputMouse :: Prism' Input (Point V2 Int32)
-- _InputClick :: Prism' Input (Point V2 Int32, MouseButton, InputMotion)
-- _InputKey :: Prism' Input Keycode
makePrisms ''Input

waitInput :: IO Input
waitInput = do
  Event _ payload <- waitEvent
  case payload of
    MouseMotionEvent event -> pure (InputMouse (mouseMotionEventPos event))
    MouseButtonEvent event ->
      pure (InputClick (mouseButtonEventPos event)
        (mouseButtonEventButton event) (mouseButtonEventMotion event))
    KeyboardEvent (KeyboardEventData _ Pressed False sym) ->
      pure (InputKey (keysymKeycode sym))
    _ -> waitInput
