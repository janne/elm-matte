module Equation exposing (Model, Msg (Choice), Answer (Unanswered, Correct, Wrong), init, update, view, isCorrect)

import Random
import Html exposing (..)
import Html.Attributes exposing (style)
import CSS exposing (..)

-- MODEL

type alias Model = {
  n1 : Int,
  op: Operator,
  n2 : Int,
  choice : Maybe Int,
  answer : Int
}


type Answer
  = Unanswered
  | Correct
  | Wrong


type alias Operator = {
  func: (Int -> Int -> Int),
  symbol: String
}


plus : Operator
plus =
  Operator (+) "+"


minus : Operator
minus =
  Operator (-) "-"


init : (Model, Cmd Msg)
init =
  (
    { n1 = 0
    , op = plus
    , n2 = 0
    , choice = Nothing
    , answer = 0
    },
    Random.generate New equation
  )


isCorrect : Model -> Answer
isCorrect model =
  case model.choice of
    Nothing ->
      Unanswered

    Just n ->
      if n == model.answer then Correct else Wrong


-- UPDATE

type Msg
  = New Model
  | Choice Int


equation : Random.Generator Model
equation =
  let
    opGenerator = Random.map (\n -> if n == 0 then plus else minus) (Random.int 0 1)
    numGenerator = Random.int 0 20
    values = Random.map3 (,,) numGenerator opGenerator numGenerator
  in
    Random.map (\(n1, op, n2) ->
      { n1 = n1
      , op = op
      , n2 = n2
      , choice = Nothing
      , answer = op.func n1 n2
      }) values


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    New newModel ->
      if newModel.answer >= 0 && newModel.answer <= 20 then
        (newModel, Cmd.none)
      else
        (model, Random.generate New equation)

    Choice n ->
      ({ model | choice = Just n }, Cmd.none)


-- VIEW

spanStyle : Style
spanStyle =
  [
    padding 10,
    margin "4px",
    display "inline-block",
    borderWidth 1,
    borderColor "#ccc",
    borderStyle "solid",
    borderRadius 20,
    backgroundColor "#eea",
    fontSize 24,
    width 40,
    height 40,
    textAlign "center",
    fontFamily "arial,sans-serif"
  ]


toList : Model -> List String
toList model =
  [ toString model.n1
  , model.op.symbol
  , toString model.n2
  , "="
  ]


-- view : Model -> Html Cmd
view model =
  let
    toSpan str = span [ style spanStyle ] [ text str ]
    response = case model.choice of
      Just n -> toString n
      Nothing -> "?"
    spans = List.map toSpan (toList model)
    correctStyle = case model.choice of
      Just n ->
        [ backgroundColor (if n == model.answer then "#5f5" else "#f55") ]
      Nothing ->
        []
  in
    div [] (spans ++ [ span [ styles [ correctStyle, spanStyle ]  ] [ text response ] ])
