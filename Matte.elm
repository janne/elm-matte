module Matte exposing (..)

import Html exposing (..)
import List
import Html.Attributes exposing (id, style, href)
import Html.Events exposing (onClick)
import Html.App as Html
import CSS exposing (..)
import Maybe
import String exposing (pad)
import Time exposing (Time, second)
import Random
import Task

main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL

type alias Model = {
  equation : Equation,
  choice : Maybe Int,
  correct : Maybe Bool,
  time : Int,
  errors : Int,
  corrects : Int,
  done : Bool
}


init : (Model, Cmd Msg)
init =
  update Next initialModel


type alias Equation = {
  const1 : Int,
  operator: Operator,
  const2 : Int,
  answer : Int
}


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


initEquation : Int -> Operator -> Int -> Equation
initEquation n1 op n2 =
  let
    answer = op.func n1 n2
  in
    {
      const1 = n1,
      operator = op,
      const2 = n2,
      answer = answer
    }



initialModel : Model
initialModel =
  {
    equation = initEquation 0 plus 0,
    choice = Nothing,
    correct = Nothing,
    time = 0,
    errors = 0,
    corrects = 0,
    done = False
  }


-- UPDATE

type Msg
  = Restart
  | Choice Int
  | NewEquation (Int, Int, Int)
  | Next
  | Tick


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Restart ->
      init

    Choice n ->
      let
        correct = n == model.equation.answer
      in
        (
          { model |
            choice = Just n,
            correct = Just correct,
            errors = if correct then model.errors else model.errors + 1,
            corrects = if correct then model.corrects + 1 else model.corrects
          }
          , Cmd.none
        )

    Next ->
      let
        g21 = (Random.int 0 20)
        g2 = (Random.int 0 1)
        gen = Random.map3 (,,) g21 g2 g21
      in
        (model, (Random.generate NewEquation gen))

    NewEquation (n1, op, n2) ->
      let
        operand = if op == 0 then plus else minus
        equation = initEquation n1 operand n2
        answer = operand.func n1 n2
      in
        if answer >= 0 && answer <= 20 then
          ({ model |
            equation = equation,
            choice = Nothing,
            correct = Nothing
          }, Cmd.none)
        else
          update Next model

    Tick ->
      ({ model |
        time = if model.done then model.time else model.time + 1,
        done = model.time >= 60*15
      }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (always Tick)


--VIEW

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


formatEquation : Equation -> List String
formatEquation equation =
  [ toString equation.const1
  , equation.operator.symbol
  , toString equation.const2
  , "="
  ]


viewEquation : Model -> Html Msg
viewEquation model =
  let
    toSpan str = span [ style spanStyle ] [ text str ]
    response = case model.choice of
      Just n -> toString n
      Nothing -> "?"
    spans = List.map toSpan (formatEquation model.equation)
    correctStyle = case model.correct of
      Just True ->
        [ backgroundColor "#5f5" ]
      Just False ->
        [ backgroundColor "#f55" ]
      Nothing ->
        []
  in
    div [] (spans ++ [ span [ styles [ correctStyle, spanStyle ]  ] [ text response ] ])


viewAlternatives : Html Msg
viewAlternatives =
  let
    toLink num = a [
      href "#",
      styles [[backgroundColor "#eae"], spanStyle ],
      onClick (Choice num)
    ] [ toString num |> text ]
  in
    span [] [
      h2 [] [ text "Vilket svar är rätt?" ],
      div [ id "alternatives" ] ( List.map toLink [0..20] )
    ]


next : Model -> Html Msg
next model =
  case model.correct of
    Just True ->
      if model.done then
         span [] [
           span [ style [ fontSize 24 ] ] [ text  "Du är klar!" ],
           div [] [
             button [
               style [ fontSize 24 ],
               onClick Restart
             ] [ text "Börja om" ]
           ]
         ]
      else
        button [
          style [ fontSize 24, marginTop 20 ],
          onClick Next
        ] [ text "Nästa" ]
    Just False ->
      span [ style [ fontSize 24 ] ] [ text  "Tyvärr, fel." ]
    Nothing ->
      text ""


zeroPad : Int -> String
zeroPad n =
  let
    str = toString n
  in
    if n < 10 then pad 2 '0' str else str


fmtTime : Int -> String
fmtTime n =
  let
    minutes = n // 60 |> zeroPad
    seconds = n % 60 |> zeroPad
  in
    minutes ++ ":" ++ seconds


view : Model -> Html Msg
view model =
  div [
    style [ maxWidth 800, margin "0 auto" ]
  ] [
    h2 [] [
      text <| "Tid: ",
      span [ style [ color "#55f" ] ] [ fmtTime model.time |> text ]
    ],
    h2 [] [
      text <| "Antal rätt: ",
      span [ style [ color "#5f5" ] ] [ model.corrects |> toString |> text ]
    ],
    h2 [] [
      text <| "Antal fel: ",
      span [ style [ color "#f55" ] ] [ model.errors |> toString |> text ]
    ],
    viewEquation model,
    viewAlternatives,
    next model
  ]
