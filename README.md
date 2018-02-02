# Purescript Bonsai

Functional web programming in Purescript.  Heavily inspired by Elm & using
the Elm Virtual DOM.

*STATUS*: Functional, but unstable API - the breaking changes are still coming.

It does have:

* Elm's Virtual DOM as backend
* Event/command system for functional updates to the virtual DOM
* Integration of event/command system with purecript's Eff and Aff.
  Commands can be effectful (e.g. accessing local storage) or
  even asynchronous (e.g. an ajax request, or a delayed update to the
  page).
* Smolder-style Html API that renders to the VirtualDom

## Documentation and examples

* A tutorial with simple examples is at http://purescript-bonsai-docs.readthedocs.io/en/latest/
* Reference documentation https://pursuit.purescript.org/packages/purescript-bonsai/
* An example application is at https://github.com/grmble/purescript-bonsai-todo

## Getting started

Start a fresh project

```sh
mkdir your-project
cd your-project
pulp init
bower install --save purescript-bonsai
pulp build
```

Edit src/Main.purs

```purescript
module Main where

import Prelude hiding (div)

import Bonsai
import Bonsai.Html
import Bonsai.Html.Attributes
import Bonsai.Html.Events
import Data.Maybe
import Data.Tuple

type Model = Int

data Msg
  = Inc
  | Dec

update :: forall eff. Msg -> Model -> Tuple (Cmd eff Msg) Model
update msg model = Tuple emptyCommand $
  case msg of
    Inc ->
      model + 1
    Dec ->
      model - 1

view :: Model -> VNode Msg
view model =

  render $
    div ! cls "counter" $ do
      text $ show model
      button ! cls "minus" ! onClick Inc $ do text "+"
      button ! cls "plus" ! onClick Dec $ do text "-"

main =
  ( window >>=
    program (ElementId "main") update view 0) *>
  pure unit
```

Add a index.html in your project root

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8"/>
  </head>
  <body style="padding: 2em;">
    <div id="main">Loading ...</div>
  </body>
  <script type='text/javascript' src='app.js'></script>
</html>
```

Start pulp in server mode

```sh
pulp server
```
