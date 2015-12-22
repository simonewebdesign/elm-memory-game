module Main where

import Array exposing (Array)
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Graphics.Input   as Input
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
  , sequence : Array Color
  , inputSequence : Array Color
  , state : GameState
  }

type alias Dimensions = (Int, Int)
type alias Position = (Int, Int)

type Action = NoOp | Add | Subtract | AddColor Color | ChangeGameState

type GameState = Play | Pause


initialModel : Model
initialModel =
  { color = red
  , counter = 0

  , level = 1
  , score = 0
  , sequence = Array.empty
  , inputSequence = Array.empty
  , state = Pause
  }


elementsMailbox : Signal.Mailbox Color
elementsMailbox = Signal.mailbox red


-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Add      -> { model | counter = model.counter + 1 }
    Subtract -> { model | counter = model.counter - 1 }
    
    AddColor color ->
      { model | inputSequence = Array.push color model.inputSequence }
    
    ChangeGameState ->
      case model.state of
        Play  -> { model | state = Pause }
        Pause -> { model | state = Play }

    _ ->
      model


-- VIEW

view : Dimensions -> Model -> Element
view (w, h) model =
  Collage.collage w h
    [ formSquare (w, h) red    |> Collage.move (-200, 160)  --Element.topLeft
    , formSquare (w, h) yellow |> Collage.move (200, 160)   --Element.topRight
    , formSquare (w, h) green  |> Collage.move (-200, -160) --Element.bottomLeft
    , formSquare (w, h) blue   |> Collage.move (200, -160)  --Element.bottomRight
    , showDebug True model
    ]


formSquare : Dimensions -> Color -> Collage.Form
formSquare (w, h) color =
  drawSquare (w, h) color
  |> Input.clickable (Signal.message elementsMailbox.address color)
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
    elementClicks = Signal.map AddColor elementsMailbox.signal
    space = Signal.map (\pressed ->
      if pressed 
      then ChangeGameState
      else NoOp
    ) Keyboard.space
  in
    Signal.mergeMany [arrows, clicks, elementClicks, space]
