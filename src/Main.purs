module Main where

import Bonsai
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
  Just main  <- domElementById (ElementId "main")
  cfg <- programEnv update view
  ps  <- programState main cfg 0
  queueCommand cfg (Cmd [Increment])

  ps' <- execProgram step cfg ps
  log "Final state"
  logAny ps'


type Model = Int

data Msg
  = Increment
  | Decrement

update :: Model -> Msg -> Model
update model Increment = model + 1
update model Decrement = model - 1

view :: Model -> VNode Msg
view model = text $ show model


-- logAny :: forall eff. Eff (console::CONSOLE|eff) Unit
logAny x =
  traceAny x \_ -> pure unit
