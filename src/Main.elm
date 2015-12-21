module Main where

import Array exposing (Array)
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Color exposing (Color, red, yellow, green, blue)
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
type alias Position = (Int, Int)

type Action = NoOp | Add | Subtract


initialModel : Model
initialModel =
  { color = red
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

view : Dimensions -> Model -> Element
view (w, h) model =
  Collage.collage w h
    [ formSquare (w, h) red    Element.topLeft
    , formSquare (w, h) yellow Element.topRight
    , formSquare (w, h) green  Element.bottomLeft
    , formSquare (w, h) blue   Element.bottomRight
    , showDebug True model
    ]


formSquare : Dimensions -> Color -> Element.Position -> Collage.Form
formSquare (w, h) color position =
  drawSquare (w, h) color
  |> Element.container w h position
  |> Collage.toForm


drawSquare : Dimensions -> Color -> Element
drawSquare (w, h) color =
  let
    width = w // 2
    height = h // 2
  in
    Element.empty
    |> Element.size width height
    |> Element.color color


showDebug : Bool -> Model -> Collage.Form
showDebug yes model =
  if yes then
    Element.show model
    |> Collage.toForm
    |> Collage.moveY 100
  else
    Element.empty
    |> Collage.toForm


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
