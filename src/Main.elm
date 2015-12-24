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
  , sequence : Array ID
  , inputSequence : Array ID
  , state : GameState
  , elements : List ( ID, ElementModel )
  }

type alias ID = Int
type alias ElementModel =
  { pressed : Bool
  , position : Position
  , color : Color
  }
type alias Dimensions = (Int, Int)
type alias Position = (Float, Float)

type Action
  = NoOp
  | Add
  | Subtract
  | Press ID
  | ChangeGameState

type GameState = Play | Pause

-- extensible record
--type alias Pressable a =
--  { a | id : Int, pressed : Bool }

--type alias PressableElement = (Element, { id : Int, pressed : Bool })
--type alias Pressable = { id : Int, pressed : Bool }


initialModel : Model
initialModel =
  { color = red
  , counter = 0

  , level = 1
  , score = 0
  , sequence = Array.empty
  , inputSequence = Array.empty
  , state = Pause
  , elements =
    [ ( 1, { pressed = False, position = (-200, 160), color = red } )
    , ( 2, { pressed = False, position = (200, 160), color = yellow } )
    , ( 3, { pressed = False, position = (-200, -160), color = green } )
    , ( 4, { pressed = False, position = (200, -160), color = blue } )
    ]
  }


elementIds : Signal.Mailbox Int
elementIds = Signal.mailbox 0


-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Add      -> { model | counter = model.counter + 1 }
    Subtract -> { model | counter = model.counter - 1 }

    Press id ->
      let updateElement (elemId, elemModel) =
        if elemId == id
          then (elemId, { elemModel | pressed = True})
          else (elemId, { elemModel | pressed = False})
      in
        { model |
          inputSequence = Array.push id model.inputSequence,
          elements = List.map updateElement model.elements
        }

    ChangeGameState ->
      case model.state of
        Play  -> { model | state = Pause }
        Pause -> { model | state = Play }

    _ ->
      model


-- VIEW

view : Dimensions -> Model -> Element
view (w, h) model =
  let
    elements = List.map (viewSquare (w, h)) model.elements
    debug = showDebug True model
  in
    Collage.collage w h (elements ++ [debug])


viewSquare : Dimensions -> (ID, ElementModel) -> Collage.Form
viewSquare (w, h) (id, { pressed, position, color }) =
  let
    width = w // 2
    height = h // 2
  in
    Element.empty
    |> Element.size width height
    |> Element.color color
    |> Element.opacity (if pressed then 1 else 0.2)
    |> Input.clickable (Signal.message elementIds.address id)
    |> Collage.toForm
    |> Collage.move position


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

    elementClicks = Signal.map Press elementIds.signal

    space = Signal.map (\pressed ->
      if pressed then ChangeGameState else NoOp
    ) Keyboard.space
  in
    Signal.mergeMany [arrows, clicks, elementClicks, space]
