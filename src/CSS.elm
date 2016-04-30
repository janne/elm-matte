module CSS exposing (..)

import Html.Attributes exposing (id, style)

(=>) = (,)


type alias Style =
  List (String, String)


styles list =
  List.foldl (++) [] list |> style


-- Helpers

px n = (toString n ++ "px")
pt n = (toString n ++ "pt")

backgroundColor s = "background-color" => s
borderColor s = "border-color" => s
borderRadius n = "border-radius" => px n
borderStyle s = "border-style" => s
borderWidth n = "border-width" => px n
color s = "color" => s
display s = "display" => s
fontFamily s = "font-family" => s
fontSize n = "font-size" => pt n
height n = "height" => px n
margin s = "margin" => s
marginBottom n = "margin-bototm" => px n
marginLeft n = "margin-left" => px n
marginRight n = "margin-right" => px n
marginTop n = "margin-top" => px n
maxWidth n = "max-width" => px n
minWidth n = "min-width" => px n
padding n = "padding" => px n
textAlign s = "text-align" => s
width n = "width" => px n
