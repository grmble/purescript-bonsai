-- | Bonsai DOM Helpers
module Bonsai.DOM
  ( domClearElement
  , domElementById
  , domRequestAnimationFrame
  , affElementById
  , affElementAction
  , focusCmd
  , focusSelectCmd
  )
where

import Prelude

import Bonsai.Types (Cmd, simpleTask)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff, whileE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.HTMLInputElement (select)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (RequestAnimationFrameId, document, requestAnimationFrame)
import DOM.Node.Node (firstChild, hasChildNodes, removeChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode, elementToNode)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | Gets a DOM Element by its ID
domElementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe Element)
domElementById id =
  window >>=
  document >>=
  htmlDocumentToDocument >>> documentToNonElementParentNode >>> getElementById id

-- | Request animation frame
domRequestAnimationFrame
  :: forall eff
  .  Eff (dom::DOM|eff) Unit
  -> Eff (dom::DOM|eff) RequestAnimationFrameId
domRequestAnimationFrame eff = do
  w <- window
  requestAnimationFrame eff w


-- | Cmd that will set the focus to the input field.
focusCmd :: forall eff msg. ElementId -> Cmd (dom::DOM|eff) msg
focusCmd id =
  simpleTask $ do
    affElementAction (focus <<< unsafeCoerce) id
    pure []

-- | Cmd that will set the focus and select the input field
focusSelectCmd :: forall eff msg. ElementId -> Cmd (dom::DOM|eff) msg
focusSelectCmd id =
  simpleTask $ do
    affElementAction action id
    pure []
  where
    action elem = do
      focus (unsafeCoerce elem)
      select (unsafeCoerce elem)


-- | Helper for calling effectful actions on dom nodes.
-- |
-- | This uses a short delay to let the render happen
-- | before trying to find the element.
affElementAction
  :: forall eff a
  .  (Element -> Eff (dom::DOM|eff) a)
  -> ElementId
  -> Aff (dom::DOM|eff) a
affElementAction f id = do
  delay (Milliseconds 20.0)
  elem <- affElementById id
  liftEff $ f elem

-- | Get a DOM element in a Aff context
-- |
-- | This represents failure as Aff error
affElementById :: forall eff. ElementId -> Aff (dom::DOM|eff) Element
affElementById id@(ElementId idStr) = do
  elem <- liftEff $ domElementById id
  case elem of
    Nothing ->
      throwError $ error ("no element with id " <> idStr)
    Just e ->
      pure e

-- | Clear a DOM element.
-- |
-- | Removes all child nodes.
domClearElement :: forall eff. Element -> Eff (dom::DOM|eff) Unit
domClearElement e =
  unsafePartial $
    whileE
      (hasChildNodes n)
      (do
        c <- fromJust <$> firstChild n
        removeChild c n)
  where
    n = elementToNode e
