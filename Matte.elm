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
import Equation

main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL

type alias Model = {
  equation : Equation.Model,
  time : Int,
  errors : Int,
  corrects : Int,
  done : Bool
}


init : (Model, Cmd Msg)
init =
  let
    ( equation, eqCmd ) = Equation.init
    cmd = Cmd.map EquationMsg eqCmd
  in
    ({
      equation = equation,
      time = 5 * 60,
      errors = 0,
      corrects = 0,
      done = False
    }, cmd)


-- UPDATE

type Msg
  = EquationMsg Equation.Msg
  | Tick
  | Choice Int
  | Next


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EquationMsg c ->
      let
        (eq, cmd) = Equation.update c model.equation
      in
        ({model | equation = eq}, Cmd.map EquationMsg cmd)

    Next ->
      let
        ( equation, eqCmd ) = Equation.init
        cmd = Cmd.map EquationMsg eqCmd
      in
        ({ model | equation = equation }, cmd)

    Choice n ->
      if not model.done && Equation.correct model.equation /= Just True then
        let
          (model, cmd) = update (EquationMsg (Equation.Choice n)) model
        in
          case Equation.correct model.equation of
            Nothing ->
              (model, cmd)
            Just True ->
              ({ model | corrects = model.corrects + 1 }, cmd)
            Just False ->
              ({ model | errors = model.errors + 1 }, cmd)
      else
        (model, Cmd.none)

    Tick ->
      let
        done = model.time <= 0
      in
        ({ model |
          time = if done then model.time else model.time - 1,
          done = done
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
    backgroundColor "#eae",
    fontSize 24,
    width 40,
    height 40,
    textAlign "center",
    fontFamily "arial,sans-serif"
  ]


viewAlternatives : Html Msg
viewAlternatives =
  let
    toLink num = a [
      href "#",
      style spanStyle,
      onClick (Choice num)
    ] [ toString num |> text ]
  in
    span [] [
      h2 [] [ text "Vilket svar är rätt?" ],
      div [ id "alternatives" ] ( List.map toLink [0..20] )
    ]


next : Model -> Html Msg
next model =
   if model.done then
      span [] [
        span [ style [ fontSize 24 ] ] [ text  "Du är klar!" ]
      ]
   else
     case Equation.correct model.equation of
       Just True ->
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
    Html.map EquationMsg (Equation.view model.equation),
    viewAlternatives,
    next model
  ]
