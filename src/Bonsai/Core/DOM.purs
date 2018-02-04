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

import Bonsai.DOM (DOM, ElementId, affF, elementById, focusElement, selectElementText, setLocationHash)
import Bonsai.Types (Cmd, TaskContext, delayUntilRendered, emittingTask)
import Control.Monad.Aff (Aff)
import Data.Foreign (F)


-- | Run the F when the model is clean.
-- |
-- | Any errors will be turned into exceptions.
-- |
-- | If you are trying to use this after an issueCommand
-- | when testing, use Bonsai.Core.delayUntilClean
-- | and coerce the Affs as needed.
delayF :: forall eff effT msg a. TaskContext effT msg -> (Unit -> F a) -> Aff (dom::DOM|eff) Unit
delayF ctx fa = do
  delayUntilRendered ctx
  _ <- affF (fa unit)
  pure unit


-- | DOM Side-Effect Cmd that will set the focus to the input field.
-- |
-- | This will run delayed once the current model is rendered.
focusCmd :: forall eff msg. ElementId -> Cmd (dom::DOM|eff) msg
focusCmd id =
  emittingTask \ctx ->
    delayF ctx (\_ -> elementById id ctx.document >>= focusElement)


-- | DOM Side-Effect Cmd that will set the focus and select the input field
-- |
-- | This will run delayed once the current model is rendered.
focusSelectCmd :: forall eff msg. ElementId -> Cmd (dom::DOM|eff) msg
focusSelectCmd id =
  emittingTask \ctx ->
    delayF ctx
      (\_ -> elementById id ctx.document >>= focusElement >>= selectElementText)



-- | DOM Side-Effect Cmd that will set the location hash.
-- |
-- | Note that this has to start with a # sign
locationHashCmd :: forall eff msg. String -> Cmd (dom::DOM|eff) msg
locationHashCmd hash =
  emittingTask \ctx ->
    affF $ setLocationHash hash ctx.document
