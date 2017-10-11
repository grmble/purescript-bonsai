module Bonsai.DOM.Internal
  ( domClearElement
  , domRequestAnimationFrame
  )
where

import Prelude

import Control.Monad.Eff (Eff, whileE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (RequestAnimationFrameId, requestAnimationFrame)
import DOM.Node.Node (firstChild, hasChildNodes, removeChild)
import DOM.Node.Types (Element, elementToNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)


-- | Request animation frame
domRequestAnimationFrame
  :: forall eff
  .  Eff (dom::DOM|eff) Unit
  -> Eff (dom::DOM|eff) RequestAnimationFrameId
domRequestAnimationFrame eff = do
  w <- window
  requestAnimationFrame eff w

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
