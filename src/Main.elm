module Main where

import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Text
import Color exposing (Color)
import Time
import Keyboard
import Window
import Mouse


-- MODEL

type alias Model =
  { color : Color
  , counter : Int
  }


type alias Keys = { x:Int, y:Int }

type Action = NoOp | Add | Subtract --ChangeColor | Add | Subtract


initialModel : Model
initialModel =
  { color = Color.red
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

makeSquare : Collage.Form
makeSquare =
  Collage.square 30
  |> Collage.filled Color.green


aTextElement : Element
aTextElement =
  Element.centered (Text.fromString "Hi there")


redSquare : Element
redSquare =
  Element.color Color.red aTextElement


myCollage : (Int, Int) -> List Collage.Form -> Element
myCollage (width, height) forms =
  Collage.collage width height forms


myForms : Model -> List Collage.Form
myForms model =
  [
    --Collage.toForm redSquare
  --, makeSquare
  Collage.toForm (Element.show model)
  ]


view : (Int, Int) -> Model -> Element
view (width, height) model =
  myCollage (width, height) (myForms model)


-- MAIN

main : Signal Element
main =
  Signal.map2 view Window.dimensions game



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
        1 -> Add
        _ -> NoOp

    arrows = Signal.sampleOn delta (Signal.map toAction x)
    clicks = Signal.map (always Add) Mouse.clicks
  in
    Signal.merge arrows clicks


--input : Signal (Float, Keys)
--input =
--  let
--    delta = Signal.map (\t -> t/20) (Time.fps 30)
--  in
--    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
