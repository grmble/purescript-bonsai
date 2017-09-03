module Main where

import Prelude

import Bonsai
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Reader
import Data.Maybe
import Data.Tuple
import Debug.Trace
import DOM
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.Node
import DOM.Node.NonElementParentNode
import DOM.Node.Types
import Native.VirtualDom
import Partial.Unsafe

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = unsafePartial $ do
  Just main  <- domElementById (ElementId "main")
  cfg <- programEnv main update view 0
  -- queueCommand cfg [Increment]
  -- ps' <- runReaderT step cfg
  -- logAny ps'
  log "Done."


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


-- logAny :: forall eff. Eff (console::CONSOLE|eff) Unit
logAny x =
  traceAny x \_ -> pure unit
