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

import Bonsai.DOM (ElementId, elementById, focusElement, affF, selectElement)
import Bonsai.Types (BONSAI, Cmd, emittingTask)
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Data.Foreign (F)



-- | Cmd that will set the focus to the input field.
focusCmd :: forall eff msg. ElementId -> Cmd (bonsai::BONSAI|eff) msg
focusCmd id =
  emittingTask \ctx ->
    delayF (\_ -> elementById id ctx.document >>= focusElement)



-- | Cmd that will set the focus and select the input field
focusSelectCmd :: forall eff msg. ElementId -> Cmd (bonsai::BONSAI|eff) msg
focusSelectCmd id =
  emittingTask \ctx ->
    delayF (\_ -> elementById id ctx.document >>= focusElement >>= selectElement)


-- | Run the F when the model is clean.
-- |
-- | Any errors will be turned into exceptions.
delayF :: forall eff a. (Unit -> F a) -> Aff (bonsai::BONSAI|eff) Unit
delayF fa = do
  delay (Milliseconds 20.0)
  _ <- affF (fa unit)
  pure unit
