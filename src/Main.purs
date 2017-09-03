module Main where

import Prelude

import Bonsai (program)
import Bonsai.DOM (domElementById)
import Bonsai.VirtualDom (VNode, node, on, text)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff (console::CONSOLE,dom::DOM,ref::REF| e) Unit
main = unsafePartial $ do
  Just mainDiv  <- domElementById (ElementId "main")
  _ <- program mainDiv update view 0
  log "..."


type Model = Int

data Msg
  = Increment
  | Decrement

update :: Model -> Msg -> Model
update model Increment = model + 1
update model Decrement = model - 1

view :: Model -> VNode Msg
view model =
  node "div" []
    [ text $ show model
    , node "button" [ on "click" $ \_ -> [Increment] ] [ text "+" ]
    , node "button" [ on "click" $ \_ -> [Decrement] ] [ text "-" ]
    ]
