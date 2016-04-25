module Matte where

import Html exposing (..)
import List
import Html.Attributes exposing (id, style, href)
import Html.Events exposing (onClick)
import CSS exposing (..)
import Signal exposing (Address, Mailbox)
import Maybe
import String exposing (pad)
import Time exposing (Time, second)
import Random

-- MODEL

type alias Model = {
  seed : Random.Seed,
  equation : Equation,
  choice : Maybe Int,
  correct : Maybe Bool,
  time : Int,
  errors : Int,
  corrects : Int,
  done : Bool
}


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


randomOperator : Random.Seed -> (Operator, Random.Seed)
randomOperator seed =
  let
    number = Random.int 0 1
    (n, seed') = Random.generate number seed
  in
     case n of
       0 -> (plus, seed')
       _ -> (minus, seed')


newEquation : Random.Seed -> (Equation, Random.Seed)
newEquation seed =
  let
    number = Random.int 0 20
    (n1, seed') = Random.generate number seed
    (n2, seed'') = Random.generate number seed'
    (op, seed''') = randomOperator(seed)
    answer = op.func n1 n2
  in
    if answer >= 0 && answer <= 20 then
      ({
        const1 = n1,
        operator = op,
        const2 = n2,
        answer = answer
      }, seed''')
    else
      newEquation seed'''


initialModel : Random.Seed -> Model
initialModel seed =
  let
    (equation, seed') = newEquation seed
  in
    {
      seed = seed',
      equation = equation,
      choice = Nothing,
      correct = Nothing,
      time = 0,
      errors = 0,
      corrects = 0,
      done = False
    }


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


viewEquation : Model -> Html
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


viewAlternatives : Address Action -> Html
viewAlternatives address =
  let
    toLink num = a [
      href "#",
      styles [[backgroundColor "#eae"], spanStyle ],
      onClick address (Choice num)
    ] [ toString num |> text ]
  in
    span [] [
      h2 [] [ text "Vilket svar är rätt?" ],
      div [ id "alternatives" ] ( List.map toLink [0..20] )
    ]


next : Address Action -> Model -> Html
next address model =
  case model.correct of
    Just True ->
      if model.done then
         span [] [
           span [ style [ fontSize 24 ] ] [ text  "Du är klar!" ],
           div [] [
             button [
               style [ fontSize 24 ],
               onClick address Restart
             ] [ text "Börja om" ]
           ]
         ]
      else
        button [
          style [ fontSize 24, marginTop 20 ],
          onClick address Next
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


view : Address Action -> Model -> Html
view address model =
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
    viewAlternatives address,
    next address model
  ]


-- UPDATE

type Action
  = NoOp
  | Restart
  | Choice Int
  | Next
  | Tick


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Restart ->
      initialModel model.seed
    Choice n ->
      let
        correct = n == model.equation.answer
      in
        { model |
          choice = Just n,
          correct = Just correct,
          errors = if correct then model.errors else model.errors + 1,
          corrects = if correct then model.corrects + 1 else model.corrects
        }
    Next ->
      let
        (equation, seed) = newEquation model.seed
      in
        { model |
          equation = equation,
          seed = seed,
          choice = Nothing,
          correct = Nothing
        }
    Tick ->
      { model |
        time = if model.done then model.time else model.time + 1,
        done = model.time >= 60*15
      }


-- SIGNALS

inbox : Mailbox Action
inbox =
  Signal.mailbox NoOp


actions : Signal Action
actions =
  Signal.merge inbox.signal timer


model : Signal Model
model =
  let
    seed = Random.initialSeed 1
  in
    Signal.foldp update (initialModel seed) actions


timer : Signal Action
timer =
  Signal.map (always Tick) (Time.every second)


-- MAIN

main : Signal Html
main =
  Signal.map (view inbox.address) model
