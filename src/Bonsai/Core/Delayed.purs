-- | Delayed cmds for DOM effects
-- |
-- | These are effectful changes to the DOM
-- | that need to run AFTER their model state
-- | has been rendered.

module Bonsai.Core.Delayed
  ( delayF
  , focusCmd
  , focusSelectCmd
  )
where

import Prelude

import Bonsai.DOM (DOM, ElementId, elementById, focusElement, affF, selectElementText)
import Bonsai.Types (Cmd, emittingTask)
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Data.Foreign (F)



-- | Cmd that will set the focus to the input field.
focusCmd :: forall eff msg. ElementId -> Cmd (dom::DOM|eff) msg
focusCmd id =
  emittingTask \ctx ->
    delayF (\_ -> elementById id ctx.document >>= focusElement)



-- | Cmd that will set the focus and select the input field
focusSelectCmd :: forall eff msg. ElementId -> Cmd (dom::DOM|eff) msg
focusSelectCmd id =
  emittingTask \ctx ->
    delayF (\_ -> elementById id ctx.document >>= focusElement >>= selectElementText)


-- | Run the F when the model is clean.
-- |
-- | Any errors will be turned into exceptions.
delayF :: forall eff a. (Unit -> F a) -> Aff (dom::DOM|eff) Unit
delayF fa = do
  delay (Milliseconds 20.0)
  _ <- affF (fa unit)
  pure unit
