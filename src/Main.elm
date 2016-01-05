module Main where

import Array exposing (Array)
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Graphics.Input   as Input
import Color exposing (Color, red, yellow, green, blue, white)
import Keyboard
import Random
import Maybe
import Task exposing (Task)
import Time exposing (Time)
import Text
import Html exposing (Html)
import Generators
import Button exposing (Button)
import Effects exposing (Effects, Never)
import StartApp

type alias ID = Int

type alias Dimensions = (Int, Int)

type Action
  = NoOp
  | Press ID
  | ChangeGameState
  | PlaySequence
  | Tick Time

type GameState = Play | Pause

type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time }

-- MODEL

type alias Model =
  { level : Int
  , score : Int
  , sequence : Array ID
  , inputSequence : Array ID
  , seed : Random.Seed
  , state : GameState
  , animationState: AnimationState
  , buttons : List ( ID, Button )
  }

initialModel : Model
initialModel =
  { level = 1
  , score = 0
  , sequence = Generators.initialSequence
  , inputSequence = Array.empty
  , seed = Random.initialSeed 111
  , state = Pause
  , animationState = Nothing
  , buttons =
    [ ( 1, { pressed = False, position = (-200, 160),  color = red } )
    , ( 2, { pressed = False, position = (200, 160),   color = yellow } )
    , ( 3, { pressed = False, position = (-200, -160), color = green } )
    , ( 4, { pressed = False, position = (200, -160),  color = blue } )
    ]
  }


animationDuration : Time
animationDuration = Time.second


noFx : Model -> ( Model, Effects Action )
noFx model =
  ( model, Effects.none )


buttonIDs : Signal.Mailbox Int
buttonIDs = Signal.mailbox 0


-- UPDATE

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      noFx model

    Press id ->
      let
        index = Array.length model.inputSequence
        sequenceID = Maybe.withDefault 0 (Array.get index model.sequence)
      in
        if id == sequenceID then
          -- correct! update
          if Array.length model.inputSequence == Array.length model.sequence - 1 then
            let
              newModel =
                model
                |> nextLevel
                |> resetButtons
                |> incrementScore
                |> resetInputSequence
                |> newSequence
            in
              noFx newModel
          else
            noFx (updateInputSequence id model)
        else
          -- wrong! reset
          noFx (reset model)


    ChangeGameState ->
      case model.state of
        Play  ->
          let newModel = { model | state = Pause }
          in noFx newModel

        Pause ->
          let newModel = { model | state = Play }
          in noFx newModel

    PlaySequence ->
      case model.animationState of
        Nothing ->
          ( model, Effects.tick Tick )

        Just _ ->
          ( model, Effects.none )

    Tick clockTime ->
      let
        newElapsedTime =
          case model.animationState of
            Nothing ->
              0

            Just {elapsedTime, prevClockTime} ->
              elapsedTime + (clockTime - prevClockTime)
      in
        if newElapsedTime > animationDuration then
          ( { model
              | level = 9001
              , animationState = Nothing
            }
          , Effects.none
          )
        else
          ( { model
              | level = 30165
              , animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
            }
          , Effects.tick Tick
          )


reset : Model -> Model
reset model =
  initialModel


nextLevel : Model -> Model
nextLevel model =
  { model | level = model.level + 1 }


resetButtons : Model -> Model
resetButtons model =
  { model | buttons = initialModel.buttons }


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

view : Signal.Address Action -> Model -> Html
view address model =
  let
    buttons = List.map (viewSquare (300, 300)) model.buttons
    debug = showDebug True model
  in
    Collage.collage 1300 480 (buttons ++ [debug, viewScore model, viewLevel model])
    |> Html.fromElement


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
  |> Collage.move (200, 200)


viewLevel : Model -> Collage.Form
viewLevel model =
  "Level: " ++ toString model.level
  |> Text.fromString
  |> Text.color white
  |> Element.rightAligned
  |> Collage.toForm
  |> Collage.move (-200, 200)


showDebug : Bool -> Model -> Collage.Form
showDebug yes model =
  if yes then
    Element.show model
    |> Collage.toForm
  else
    Element.empty
    |> Collage.toForm


-- MAIN

main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = noFx initialModel
    , update = update
    , view = view
    , inputs = inputs
    }


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


inputs : List (Signal Action)
inputs =
  let
    buttonClicks = Signal.map Press buttonIDs.signal

    space = Signal.map (\pressed ->
      if pressed then PlaySequence else NoOp
    ) Keyboard.space
  in
    [buttonClicks, space]
