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

type Action = NoOp | Add | Subtract --ChangeColor | Add | Subtract


initialModel : Model
initialModel =
  { color = red
  , counter = 0
  }


-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Add ->
      { model | counter = model.counter + 1 }
    Subtract ->
      { model | counter = model.counter - 1 }
    _ ->
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


myForms : Model -> List Collage.Form
myForms model =
  [
    --Collage.toForm redSquare
  --, makeSquare
  Collage.toForm (Element.show model)
  ]


view : Model -> Element.Element
view model =
  myCollage (myForms model)


-- MAIN

main : Signal Element.Element
main =
  Signal.map view game



-- SIGNALS

game : Signal Model
game =
  Signal.foldp update initialModel input


input : Signal Action
input =
  let
    x = Signal.map .x Keyboard.arrows
    delta = Time.fps 30
    toAction n =
      case n of
        -1 -> Subtract
        0 -> NoOp
        1 -> Add
        _ -> NoOp

    actions = Signal.map toAction x
  in
    Signal.sampleOn delta actions


--input : Signal (Float, Keys)
--input =
--  let
--    delta = Signal.map (\t -> t/20) (Time.fps 30)
--  in
--    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
