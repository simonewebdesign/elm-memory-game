module Main where

import Graphics.Element as Element
import Graphics.Collage as Collage
import Text
import Color exposing (Color, rgb)
import Time
import Keyboard
--import Window

-- MODEL

type alias Model =
  { color : Color
  , counter : Int
  }


type alias Keys = { x:Int, y:Int }


model : Model
model =
  { color = red
  , counter = 0
  }


-- UPDATE

update : Model -> Model
update model =
  model


-- VIEW

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


view : Model -> Element.Element
view model =
  myCollage myForms


-- MAIN

--main : Element.Element
--main =
--  myCollage myForms


-- SIGNALS

main : Signal Element.Element
main =
  Signal.map2 view Signal model (Signal.foldp update model input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (Time.fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
