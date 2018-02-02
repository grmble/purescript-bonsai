-- | Bonsai DOM Helpers
-- |
-- | Provides access to the needed bits of the DOM.
-- |
-- | It provides access to the global javascript Window
-- | and Document objects, but it never uses those globals.
-- | If a functions needs Window or Document, it takes a parameter
-- | of that type.
-- |
-- | This means the module can be used with JSDOM in a node
-- | process, e.g. for testing or server side rendering.
module Bonsai.DOM
  ( ElementId(..)
  , RequestAnimationFrameId(..)
  , affElementAction
  , affElementById
  , appendChild
  , clearElement
  , document
  , elementById
  , focusCmd
  , focusElement
  , focusSelectCmd
  , requestAnimationFrame
  , selectInputElementText
  , textContent
  , window
  )
where

import Prelude

import Bonsai.Types (BONSAI, Cmd, Document, Element(..), Window, emittingTask)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Foreign (Foreign, isNull, isUndefined)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))



-- | The ID of an element in the DOM
newtype ElementId =
  ElementId String

-- | The ID of a request animation frame
newtype RequestAnimationFrameId =
  RequestAnimationFrameId String

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

-- | Get the element's text content
foreign import textContent :: forall eff. Element -> Eff (bonsai::BONSAI|eff) String

maybeNull :: forall eff a. (Foreign -> a) -> Foreign -> Eff (bonsai::BONSAI|eff) (Maybe a)
maybeNull fn f =
  pure
    if (isNull f) || (isUndefined f)
      then Nothing
      else Just $ fn f


-- | Cmd that will set the focus to the input field.
focusCmd :: forall eff msg. ElementId -> Cmd (bonsai::BONSAI|eff) msg
focusCmd id =
  emittingTask \ctx ->
    affElementAction focusElement id ctx.document *> pure unit


-- | Cmd that will set the focus and select the input field
focusSelectCmd :: forall eff msg. ElementId -> Cmd (bonsai::BONSAI|eff) msg
focusSelectCmd id =
  emittingTask \ctx ->
    affElementAction action id ctx.document *> pure unit
  where
    action elem =
      focusElement elem *>
      selectInputElementText elem


-- | Helper for calling effectful actions on dom nodes.
-- |
-- | This uses a short delay to let the render happen
-- | before trying to find the element.
affElementAction
  :: forall eff a
  .  (Element-> Eff (bonsai::BONSAI|eff) a)
  -> ElementId
  -> Document
  -> Aff (bonsai::BONSAI|eff) a
affElementAction f id doc = do
  delay (Milliseconds 20.0)
  elem <- affElementById id doc
  liftEff $ f elem

-- | Get a DOM element in a Aff context
-- |
-- | This represents failure as Aff error
affElementById :: forall eff. ElementId -> Document -> Aff (bonsai::BONSAI|eff) Element
affElementById id@(ElementId idStr) doc = do
  elem <- liftEff $ elementById id doc
  case elem of
    Nothing ->
      throwError $ error ("no element with id " <> idStr)
    Just e ->
      pure e
