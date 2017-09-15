# Purescript Bonsai

Functional web programming in Purescript.  Heavily inspired by Elm & using
the Elm Virtual DOM.

Not really ready for public consumption.  Things will change and
break in major ways.  No support for HTML yet - you have to write
out all your virtual DOM nodes by hand.  No Ajax requests, no
local storage.  You name it, we ain't got it ...  yet.

## Getting started

Start a fresh project

    mkdir your-project
    cd your-project
    pulp init
    bower install --save purescript-bonsai
    pulp build

Edit src/Main.purs

    module Main where

    import Prelude

    import Bonsai
    import Bonsai.Event
    import Data.Maybe
    import DOM
    import DOM.Node.Types
    import Partial.Unsafe (unsafePartial)

    type Model = Int

    data Msg
      = Inc
      | Dec

    update :: Model -> Msg -> UpdateResult Model Msg
    update model msg = plainResult $
      case msg of
        Inc ->
          model + 1
        Dec ->
          model - 1

    view :: Model -> VNode Msg
    view model =
      node "div" []
        [ text $ show model
        , node "button"
            [ onClick Inc ]
            [ text "+"]
        , node "button"
            [ onClick Dec ]
            [ text "-" ]
        ]


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
