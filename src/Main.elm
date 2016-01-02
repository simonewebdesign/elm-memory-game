module Main where

import Array exposing (Array)
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Graphics.Input   as Input
import Color exposing (Color, red, yellow, green, blue, white)
import Time
import Keyboard
import Window
import Random
import Maybe
import Text
import Generators
import Button exposing (Button)

-- MODEL

type alias Model =
  { level : Int
  , score : Int
  , sequence : Array ID
  , inputSequence : Array ID
  , seed : Random.Seed
  , state : GameState
  , buttons : List ( ID, Button )
  }

type alias ID = Int

type alias Dimensions = (Int, Int)

type Action
  = NoOp
  | Press ID
  | ChangeGameState

type GameState = Play | Pause


initialModel : Model
initialModel =
  { level = 1
  , score = 0
  , sequence = Generators.initialSequence
  , inputSequence = Array.empty
  , seed = Random.initialSeed 111
  , state = Pause
  , buttons =
    [ ( 1, { pressed = False, position = (-200, 160),  color = red } )
    , ( 2, { pressed = False, position = (200, 160),   color = yellow } )
    , ( 3, { pressed = False, position = (-200, -160), color = green } )
    , ( 4, { pressed = False, position = (200, -160),  color = blue } )
    ]
  }


buttonIDs : Signal.Mailbox Int
buttonIDs = Signal.mailbox 0


-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Press id ->
      let
        index = Array.length model.inputSequence
        sequenceID = Maybe.withDefault 0 (Array.get index model.sequence)
      in
        if id == sequenceID then
          -- correct! update
          if Array.length model.inputSequence == Array.length model.sequence - 1 then
            model
            |> nextLevel
            |> resetButtons
            |> incrementScore
            |> resetInputSequence
            |> newSequence
          else
            updateInputSequence id model
        else
          -- wrong! reset
          reset model


    ChangeGameState ->
      case model.state of
        Play  -> { model | state = Pause }
        Pause -> { model | state = Play }


reset : Model -> Model
reset model =
  initialModel


nextLevel : Model -> Model
nextLevel model =
  { model | level = model.level + 1 }


resetButtons : Model -> Model
resetButtons model =
  let
    unpush button =
      case button of
        ( id, btnModel ) ->
          ( id, { btnModel | pressed = False } )
  in
    { model | buttons = List.map unpush model.buttons }


incrementScore : Model -> Model
incrementScore model =
  { model | score = model.score + 3 ^ model.level }


resetInputSequence : Model -> Model
resetInputSequence model =
  { model | inputSequence = initialModel.inputSequence }


updateInputSequence : ID -> Model -> Model
updateInputSequence id model =
  let
    updateElement (elemId, elemModel) =
      if elemId == id
        then (elemId, { elemModel | pressed = True })
        else (elemId, { elemModel | pressed = False })
  in
    { model
      | inputSequence = Array.push id model.inputSequence
      , buttons = List.map updateElement model.buttons
    }


newSequence : Model -> Model
newSequence model =
  let
    (newID, newSeed) = Generators.randomID model.seed
  in
  { model
    | sequence = Array.push newID model.sequence
    , seed = newSeed
  }


-- VIEW

view : Dimensions -> Model -> Element
view (w, h) model =
  let
    buttons = List.map (viewSquare (w, h)) model.buttons
    debug = showDebug True model
  in
    Collage.collage w h (buttons ++ [debug, viewScore model, viewLevel model])


viewSquare : Dimensions -> (ID, Button) -> Collage.Form
viewSquare (w, h) (id, { pressed, position, color }) =
  let
    width = w // 2
    height = h // 2
  in
    Element.empty
    |> Element.size width height
    |> Element.color color
    |> Element.opacity (if pressed then 1 else 0.2)
    |> Input.clickable (Signal.message buttonIDs.address id)
    |> Collage.toForm
    |> Collage.move position


viewScore : Model -> Collage.Form
viewScore model =
  "Score: " ++ toString model.score
  |> Text.fromString
  |> Text.color white
  |> Element.rightAligned
  |> Collage.toForm
  |> Collage.move (300, 300)


viewLevel : Model -> Collage.Form
viewLevel model =
  "Level: " ++ toString model.level
  |> Text.fromString
  |> Text.color white
  |> Element.rightAligned
  |> Collage.toForm
  |> Collage.move (-300, 300)


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
    delta = Time.fps 30

    buttonClicks = Signal.map Press buttonIDs.signal

    space = Signal.map (\pressed ->
      if pressed then ChangeGameState else NoOp
    ) Keyboard.space
  in
    Signal.mergeMany [buttonClicks, space]
