module Main where

import Array exposing (Array)
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

  , level : Int
  , score : Int
  , sequence : Array String
  }

type alias Dimensions = (Int, Int)

type Action = NoOp | Add | Subtract


initialModel : Model
initialModel =
  { color = Color.red
  , counter = 0

  , level = 1
  , score = 0
  , sequence = Array.empty
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


drawSquare : Dimensions -> Color -> Collage.Form
drawSquare (width, height) color =
  Collage.square (toFloat (height // 2))
  |> Collage.filled color


view : Dimensions -> Model -> Element
view (width, height) model =
  Collage.collage width height
    [ drawSquare (width, height) Color.red
    , drawSquare (width, height) Color.yellow
    , drawSquare (width, height) Color.green
    , drawSquare (width, height) Color.blue
    , Collage.toForm (Element.show model) -- debugging
    ]

-- MAIN

main : Signal Element
main =
  Signal.map2 view Window.dimensions game


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
