module Main where

import Graphics.Element as Element
import Graphics.Collage as Collage
import Text
import Color exposing (Color, rgb)


red : Color
red =
  rgb 255 0 0


green : Color
green =
  rgb 0 255 0


makeSquare : Collage.Form
makeSquare =
  Collage.square 30
  |> Collage.filled green


aTextElement : Element.Element
aTextElement =
  Element.centered (Text.fromString "Hi there")


redSquare : Element.Element
redSquare =
  Element.color red aTextElement


myCollage : List Collage.Form -> Element.Element
myCollage forms =
  Collage.collage 640 480 forms


myForms : List Collage.Form
myForms =
  [ Collage.toForm redSquare
  , makeSquare
  ]


main : Element.Element
main =
  myCollage myForms
