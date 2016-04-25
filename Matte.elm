module Matte where

import Html exposing (..)
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

format : Equation -> Html
format equation =
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
    div [] (List.map toSpan arr)

plus : Operator
plus =
  Operator (+) "+"

main : Html
main =
  format <| Equation 10 plus 20
