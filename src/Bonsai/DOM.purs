-- | Bonsai DOM Helpers
module Bonsai.DOM
  ( domElementById
  , domRequestAnimationFrame
  )
where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (RequestAnimationFrameId, document, requestAnimationFrame)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId, documentToNonElementParentNode)
import Data.Maybe (Maybe)

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
