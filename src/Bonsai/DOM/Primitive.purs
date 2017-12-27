-- | Primitive DOM Helpers
-- |
-- | This module is used from Bonsai.VirtualDom, so it can't depend on anything
-- | except Bonsai.Types
-- |
-- | Since the VirtualDom does most of the DOM handling,
-- | only some functionality is needed in addition to that
-- | (e.g. elementById, setting the focus, ...)
-- |
-- | This module does not distinguish DOM nodes and elements.
-- | If you need additional functionaliy, Element is just
-- | a newtype on a foreign - just use it with a different DOM
-- | module.
module Bonsai.DOM.Primitive
  ( Element(..)
  , ElementId(..)
  , RequestAnimationFrameId(..)
  , appendChild
  , clearElement
  , document
  , elementById
  , focusElement
  , requestAnimationFrame
  , selectInputElementText
  , window
  )
where

import Prelude

import Bonsai.Types (BONSAI, Document, Window)
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign, isNull, isUndefined)
import Data.Maybe (Maybe(..))

-- | The ID of an element in the DOM
newtype ElementId =
  ElementId String

-- | The ID of a request animation frame
newtype RequestAnimationFrameId =
  RequestAnimationFrameId String

-- | A DOM Element
newtype Element =
  Element Foreign

-- | Get the global javascript Window object
foreign import window
  :: forall eff
  .  Eff (bonsai::BONSAI|eff) Window

-- | Get the global javascript Document object
foreign import document
  :: forall eff
  .  Window -> Eff (bonsai::BONSAI|eff) Document


foreign import primElementById
  :: forall eff
  .  ElementId -> Document -> Eff (bonsai::BONSAI | eff) Foreign

-- | Get the element identified by id.
elementById :: forall eff. ElementId -> Document -> Eff (bonsai::BONSAI| eff) (Maybe Element)
elementById id doc =
  primElementById id doc >>= maybeNull Element


-- | Append the child
-- |
-- | The container/parent is the second argument!
foreign import appendChild
  :: forall eff
  .  Element -> Element -> Eff (bonsai::BONSAI|eff) Unit

-- | Clear the element.
-- |
-- | Removes all child nodes of the element.
foreign import clearElement :: forall eff. Element -> Eff (bonsai::BONSAI|eff) Unit

-- | Focus the element
foreign import focusElement :: forall eff. Element -> Eff (bonsai::BONSAI|eff) Unit

-- | Request an animation frame.
foreign import requestAnimationFrame
  :: forall eff
  .  Eff (bonsai::BONSAI|eff) Unit
  -> Window
  -> Eff (bonsai::BONSAI|eff) RequestAnimationFrameId

-- | Select the input elements text.
foreign import selectInputElementText :: forall eff. Element -> Eff (bonsai::BONSAI|eff) Unit

maybeNull :: forall eff a. (Foreign -> a) -> Foreign -> Eff (bonsai::BONSAI|eff) (Maybe a)
maybeNull fn f =
  pure
    if (isNull f) || (isUndefined f)
      then Nothing
      else Just $ fn f
