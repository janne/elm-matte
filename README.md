Matte
=====

Simple math exercises for children, built in Elm.

Setup
-----

* `npm install -g elm`
* `elm package install`
* `elm reactor`
* http://localhost:8000/src/Main.elm

Electron
--------

To create a runnable Mac OS X app.

* `elm make src/Main.elm --output elm.js`
* `npm install`
* `npm run packager`
* `open Elm\ Matte-darwin-x64`

CSS
---

This app uses inline CSS, implemented on the `Html.Attributes.style`. To check for types it's wrapped with the module `CSS` that exposes the types. Example:

```elm
import CSS exposing (..)
import Html exposing (..)
import Html.Attributes as Attributes
main =
  div [
    Attributes.style [
      width 100,
      backgroundColor "#eee",
      borderColor "#ccc",
      borderRadius 10,
      borderStyle "solid",
      borderWidth 1,
      color "#f00",
      fontSize 24,
      padding 10,
      textAlign "center"
    ]
  ] [ text "Hello!" ]
```

Screenshot
----------

![Screenshot](https://raw.githubusercontent.com/janne/elm-matte/master/assets/screenshot.jpg)

