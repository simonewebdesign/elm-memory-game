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
  | StartGame
  | Press ID
  | ChangeGameState
  --| PlaySequence
  | Tick Time

type GameState = Play | Pause

type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time, step : Int }

-- MODEL

type alias Model =
  { level : Int
  , score : Int
  , sequence : Array ID
  , inputSequence : Array ID
  , seed : Random.Seed
  , state : GameState
  , isGameOver : Bool
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
  , isGameOver = False
  , animationState = Nothing
  , buttons =
    [ ( 1, { pressed = False, position = (-200, 160),  color = red } )
    , ( 2, { pressed = False, position = (200, 160),   color = yellow } )
    , ( 3, { pressed = False, position = (-200, -160), color = green } )
    , ( 4, { pressed = False, position = (200, -160),  color = blue } )
    ]
  }

animationInterval : Time
animationInterval = Time.second


noFx : Model -> ( Model, Effects Action )
noFx model =
  ( model, Effects.none )


buttonIDs : Signal.Mailbox Int
buttonIDs = Signal.mailbox 0


-- UPDATE

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      noFx model

    StartGame ->
      (,) { model | isGameOver = False } (Effects.tick Tick)

    Press id ->
      doPress id model

    ChangeGameState ->
      case model.state of
        Play  ->
          noFx { model | state = Pause }
        Pause ->
          noFx { model | state = Play }

    --PlaySequence ->
    --  case model.animationState of
    --    Nothing ->
    --      ( model, Effects.tick Tick )
    --    Just _ ->
    --      noFx model

    Tick clockTime ->
      doTick clockTime model


doPress : ID -> Model -> ( Model, Effects Action )
doPress id model =
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
      -- wrong! end the game
      noFx { model | isGameOver = True }


doTick : Time -> Model -> ( Model, Effects Action )
doTick clockTime model =
  let
    newElapsedTime =
      case model.animationState of
        Nothing ->
          0

        Just {elapsedTime, prevClockTime, step} ->
          elapsedTime + (clockTime - prevClockTime)

    currentStep =
      case model.animationState of
        Nothing ->
          0 -- the first step is the first index of the inputSequence array

        Just {elapsedTime, prevClockTime, step} ->
          step
  in
    if newElapsedTime > animationInterval then
      let
        -- get the button that has the ID that's stored in model.sequence[ currentStep ]
        -- TODO: refactor. I think it'd be simpler if you could get the button right away.
        idOfButtonToPress =
          model.sequence
          |> Array.get currentStep
          |> Maybe.withDefault 0
      in
        -- progress the animation to the next step, and
        -- highlight the button
        ( { model | animationState =
            Just { elapsedTime = 0
                 , prevClockTime = clockTime
                 , step = currentStep + 1
                 }
          }
          |> pressButton idOfButtonToPress
        , Effects.tick Tick
        )

    else if currentStep > Array.length model.sequence then
      -- end the animation
      ( { model | animationState = Nothing }
      , Effects.none
      )

    else
      -- animation is still running, send another Tick
      ( { model | animationState =
          Just { elapsedTime = newElapsedTime
               , prevClockTime = clockTime
               , step = currentStep
               }
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


pressButton : ID -> Model -> Model
pressButton id model =
  let
    maybePress (btnId, btnModel) =
      if btnId == id then
        ( btnId, { btnModel | pressed = True } )
      else
        ( btnId, { btnModel | pressed = False } )
  in
    { model | buttons = List.map maybePress model.buttons }


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
    buttons = List.map (viewButton (300, 300)) model.buttons
    debug = Element.show model |> Collage.toForm
    score = viewTopText ("Score: " ++ toString model.score) |> Collage.move (200, 200)
    level = viewTopText ("Level: " ++ toString model.level) |> Collage.move (-200, 200)
    gameOverText =
      case model.isGameOver of
        True ->
          viewCenteredText "Game Over\nPress spacebar to restart"
        False ->
          Element.empty |> Collage.toForm

    views = buttons ++ [debug, score, level, gameOverText]
  in
    Collage.collage 1300 480 views
    |> Html.fromElement


viewButton : Dimensions -> (ID, Button) -> Collage.Form
viewButton (w, h) (id, { pressed, position, color }) =
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


viewCenteredText : String -> Collage.Form
viewCenteredText str =
  Text.fromString str
  |> Text.color white
  |> Element.centered
  |> Collage.toForm


viewTopText : String -> Collage.Form
viewTopText str =
  Text.fromString str
  |> Text.color white
  |> Element.rightAligned
  |> Collage.toForm


-- MAIN

main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }


init : ( Model, Effects Action )
init =
  (,) initialModel (Effects.tick Tick)


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


inputs : List (Signal Action)
inputs =
  let
    buttonClicks = Signal.map Press buttonIDs.signal

    space = Signal.map (\pressed ->
      if pressed then StartGame else NoOp
    ) Keyboard.space
  in
    [buttonClicks, space]
