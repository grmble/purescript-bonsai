module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.Node
import DOM.Node.NonElementParentNode
import DOM.Node.Types
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Native.VirtualDom
import Partial.Unsafe
import Prelude

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = unsafePartial $ do
  log "Hello, world!"
  Just main  <- elementById (ElementId "main")
  let v1 = node "div" [] [node "h1" [] [text "Jodok"]]
  v1Dom <- renderInitial main v1
  logAny v1Dom

  let v2 = node "div" [] [ node "h1" [] [text "Simon"]
                         , node "p" [] [text "Ist das neue Baby im Haus ..."]
                         ]
  let p = diff v1 v2
  logAny p
  v2Dom <- applyPatches v1Dom v1 p

  logAny v2Dom


-- logAny :: forall eff. Eff (console::CONSOLE|eff) Unit
logAny x =
  traceAny x \_ -> pure unit

elementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe Element)
elementById id =
  window >>=
  document >>=
  htmlDocumentToDocument >>> documentToNonElementParentNode >>> getElementById id


renderInitial main node = do
  let elem = render node
  _ <- appendChild (elementToNode elem) (elementToNode main)
  pure elem
