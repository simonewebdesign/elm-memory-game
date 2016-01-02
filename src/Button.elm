module Button where

import Color exposing (Color)

type alias Button =
  { pressed : Bool
  , position : (Float, Float)
  , color : Color
  }
