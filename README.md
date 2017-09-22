# Purescript Bonsai

Functional web programming in Purescript.  Heavily inspired by Elm & using
the Elm Virtual DOM.

*DISCLAIMER*: Not ready for public consumption.  Things will change and
break in major ways.  No support for HTML yet - you have to write
out all your virtual DOM nodes by hand.

It does have:

* Elm's Virtual DOM as backend
* Event/command system for functional updates to the virtual DOM
* Integration of event/command system with purecript's Eff and Aff.
  Commands can be effectful (e.g. accessing local storage) or
  even asynchronous (e.g. an ajax request, or a delayed update to the
  page).

## Getting started

Start a fresh project

    mkdir your-project
    cd your-project
    pulp init
    bower install --save purescript-bonsai
    pulp build

Edit src/Main.purs

    module Main where

    import Prelude hiding (div)

    import Bonsai
    import Bonsai.Html
    import Bonsai.Html.Attributes
    import Bonsai.Html.Events
    import Data.Maybe
    import DOM
    import DOM.Node.Types
    import Partial.Unsafe (unsafePartial)

    type Model = Int

    data Msg
      = Inc
      | Dec

    update :: forall eff. Model -> Msg -> UpdateResult eff Model Msg
    update model msg = plainResult $
      case msg of
        Inc ->
          model + 1
        Dec ->
          model - 1

    view :: Model -> VNode Msg
    view model =

      render $
        div $ do
          text $ show model
          button ! onClick Inc $ do text "+"
          button ! onClick Dec $ do text "-"

    main = unsafePartial $ do
      Just mainDiv  <- domElementById (ElementId "main")
      _ <- program mainDiv update view 0
      pure unit


Add a index.html in your project root

    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8"/>
      </head>
      <body style="padding: 2em;">
        <div id="main"></div>
      </body>
      <script type='text/javascript' src='app.js'></script>
    </html>

Start pulp in server mode

    pulp server

## Other examples

A more involved example is at https://github.com/grmble/purescript-bonsai-todo
