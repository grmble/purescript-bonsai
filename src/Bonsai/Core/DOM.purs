-- | DOM side-effect command helpers
-- |
-- | These are effectful changes to the DOM
-- | that are not represented in the Bonsai model.


module Bonsai.Core.DOM
  ( delayF
  , focusCmd
  , focusSelectCmd
  , locationHashCmd
  )
where

import Prelude

import Bonsai.DOM (ElementId, affF, elementById, focusElement, selectElementText, setLocationHash)
import Bonsai.Types (Cmd, TaskContext, delayUntilRendered, emittingTask, unitTask)
import Effect.Aff (Aff)
import Foreign (F)


-- | Run the F when the model is clean.
-- |
-- | Any errors will be turned into exceptions.
delayF :: forall msg a. TaskContext msg -> (Unit -> F a) -> Aff Unit
delayF ctx fa = do
  delayUntilRendered ctx
  _ <- affF (fa unit)
  pure unit


-- | DOM Side-Effect Cmd that will set the focus to the input field.
-- |
-- | This will run delayed once the current model is rendered.
focusCmd :: forall msg. ElementId -> Cmd msg
focusCmd id =
  emittingTask \ctx ->
    delayF ctx (\_ -> elementById id ctx.document >>= focusElement)


-- | DOM Side-Effect Cmd that will set the focus and select the input field
-- |
-- | This will run delayed once the current model is rendered.
focusSelectCmd :: forall msg. ElementId -> Cmd msg
focusSelectCmd id =
  emittingTask \ctx ->
    delayF ctx
      (\_ -> elementById id ctx.document >>= focusElement >>= selectElementText)



-- | DOM Side-Effect Cmd that will set the location hash.
-- |
-- | Note that this has to start with a # sign
locationHashCmd :: forall msg. String -> Cmd msg
locationHashCmd hash =
  unitTask \doc ->
    affF $ setLocationHash hash doc
