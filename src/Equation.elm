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
    Random.generate New generator
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
  = New (Int, Int, Int)
  | Choice Int

generator : Random.Generator ( Int, Int, Int )
generator =
  let
    g21 = (Random.int 0 20)
    g2 = (Random.int 0 1)
  in
    Random.map3 (,,) g21 g2 g21


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    New (n1, op, n2) ->
      let
        operand = if op == 0 then plus else minus
        equation =
          { n1 = n1
          , op = operand
          , n2 = n2
          , choice = Nothing
          , answer = operand.func n1 n2
          }
      in
        if equation.answer >= 0 && equation.answer <= 20 then
          (equation, Cmd.none)
        else
          (model, Random.generate New generator)

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
