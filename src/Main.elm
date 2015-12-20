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
type alias Position = (Int, Int)

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


drawPositionedSquare : Dimensions -> Position -> Color -> Collage.Form
drawPositionedSquare (width, height) (x, y) color =
  Collage.square (toFloat (height // 2))
  |> Collage.filled color
  |> Collage.move (toFloat x, toFloat y)


drawSquare : Dimensions -> Color -> Collage.Form
drawSquare (w, h) color =
  let
    dimensions = toFloat (h // 2)
  in
    Collage.square dimensions
    |> Collage.filled color


drawSquareElement : Dimensions -> Color -> Element
drawSquareElement (w, h) color =
  let
    width = w // 2
    height = h // 2
  in
    Element.empty
    |> Element.size width height
    |> Element.color color


aContainer : Element
aContainer =
  Element.container 300 300 Element.topLeft redSquare
  |> Element.color Color.blue


containedSquare : Dimensions -> Element.Position -> Element -> Element
containedSquare (w, h) pos el =
  Element.container w h pos el
  --|> Element.color color


view : Dimensions -> Model -> Element
view (w, h) model =
  Collage.collage w h
    --[ drawSquare (w, h) ((-w // 2), (h // 2)) Color.red
    --, drawSquare (w, h) ((-w // 2), (-h // 2)) Color.yellow
    --, drawSquare (w, h) ((w // 2), (h // 2)) Color.green
    --, drawSquare (w, h) ((w // 2), (-h // 2)) Color.blue
    [ Collage.toForm (containedSquare (w, h) Element.topLeft (drawSquareElement (w, h) Color.red))
    , Collage.toForm (containedSquare (w, h) Element.topRight (drawSquareElement (w, h) Color.yellow))
    , Collage.toForm (containedSquare (w, h) Element.bottomLeft (drawSquareElement (w, h) Color.green))
    , Collage.toForm (containedSquare (w, h) Element.bottomRight (drawSquareElement (w, h) Color.blue))
    , Collage.toForm (Element.show model)
      |> Collage.moveY 100  -- debugging
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
