module Matte where

import Html exposing (..)
import Html.Attributes exposing (id)
import List

type alias Equation = {
  const1 : Int,
  operator: Operator,
  const2 : Int
}

type alias Operator = {
  func: (Int -> Int -> Int),
  symbol: String
}

format : Equation -> List Int -> Html
format equation alternatives =
  let
    arr =
      [ toString equation.const1
      , equation.operator.symbol
      , toString equation.const2
      , "="
      ]
    toSpan str =
      span [] [ text str ]
  in
    div [] [
      div [ id "equation" ] (List.map toSpan arr),
      div [ id "alternatives" ] (List.map toString alternatives |> List.map toSpan)
    ]

plus : Operator
plus =
  Operator (+) "+"

main : Html
main =
  let
    equation = Equation 3 plus 7
    alternatives = [ 9, 10, 11 ]
  in
    format equation alternatives
