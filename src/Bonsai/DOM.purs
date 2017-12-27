-- | Bonsai DOM Helpers
module Bonsai.DOM
  ( module P
  , affElementById
  , affElementAction
  , focusCmd
  , focusSelectCmd
  )
where

import Prelude

import Bonsai.Core (emittingTask)
import Bonsai.DOM.Primitive as P
import Bonsai.Types (BONSAI, Cmd)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

-- | Cmd that will set the focus to the input field.
focusCmd :: forall eff msg. P.Document -> P.ElementId -> Cmd (bonsai::BONSAI|eff) msg
focusCmd doc id =
  emittingTask \_ ->
    affElementAction P.focusElement id doc *> pure unit


-- | Cmd that will set the focus and select the input field
focusSelectCmd :: forall eff msg. P.Document -> P.ElementId -> Cmd (bonsai::BONSAI|eff) msg
focusSelectCmd doc id =
  emittingTask \_ ->
    affElementAction action id doc *> pure unit
  where
    action elem =
      P.focusElement elem *>
      P.selectInputElementText elem


-- | Helper for calling effectful actions on dom nodes.
-- |
-- | This uses a short delay to let the render happen
-- | before trying to find the element.
affElementAction
  :: forall eff a
  .  (P.Element-> Eff (bonsai::BONSAI|eff) a)
  -> P.ElementId
  -> P.Document
  -> Aff (bonsai::BONSAI|eff) a
affElementAction f id doc = do
  delay (Milliseconds 20.0)
  elem <- affElementById id doc
  liftEff $ f elem

-- | Get a DOM element in a Aff context
-- |
-- | This represents failure as Aff error
affElementById :: forall eff. P.ElementId -> P.Document -> Aff (bonsai::BONSAI|eff) P.Element
affElementById id@(P.ElementId idStr) doc = do
  elem <- liftEff $ P.elementById id doc
  case elem of
    Nothing ->
      throwError $ error ("no element with id " <> idStr)
    Just e ->
      pure e
