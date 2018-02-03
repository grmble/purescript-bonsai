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
  ( Document(..)
  , Element(..)
  , ElementId(..)
  , Window(..)
  , RequestAnimationFrameId(..)

  , failNullOrUndefined
  , affF
  , runF

  , appendChild
  , clearElement
  , document
  , elementById
  , focusElement
  , innerHTML
  , ownerDocument
  , querySelector
  , querySelectorAll
  , requestAnimationFrame
  , selectElement
  , textContent
  , window
  )
where

import Prelude

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (F, Foreign, ForeignError(..), fail, isNull, isUndefined, readString, renderForeignError)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Newtype (class Newtype, unwrap, wrap)


-- | The type for the global javascript document
newtype Document =
  Document Foreign

derive instance newtypeDocument :: Newtype Document _

-- | The type for the global javascript window
newtype Window =
  Window Foreign

derive instance newtypeWindow :: Newtype Window _


-- | The type for a dom element
newtype Element =
  Element Foreign

derive instance newtypeElement :: Newtype Element _

-- | The ID of an element in the DOM
newtype ElementId =
  ElementId String

derive instance newtypeElementId :: Newtype ElementId _

-- | The ID of a request animation frame
newtype RequestAnimationFrameId =
  RequestAnimationFrameId String

derive instance newtypeRequestAnimationFrameId :: Newtype RequestAnimationFrameId _


-- primitive methods from the native module
-- note that simple property access is done in F - no native code needed
foreign import primitives :: forall eff.
  { window :: Window
  , elementById :: Fn2 ElementId Document Foreign
  , appendChild :: Fn2 Element Element Unit
  , clearElement :: Fn1 Element Unit
  , focusElement :: Fn1 Element Unit
  , selectElement :: Fn1 Element Unit
  , querySelector :: Fn2 String Foreign Foreign
  , querySelectorAll :: Fn2 String Foreign (Array Element)
  , requestAnimationFrame :: Fn2 (Eff eff Unit) Window RequestAnimationFrameId
  }


-- | Fail the F if the forein value is null or undefined
failNullOrUndefined :: String -> Foreign -> F Foreign
failNullOrUndefined msg x =
  if isNull x || isUndefined x
    then fail $ ForeignError ("null or undefined: " <> msg)
    else pure x

-- | Run the F in Eff
-- |
-- | Errors will be thrown as exception
runF :: forall eff a. F a -> Eff (exception::EXCEPTION|eff) a
runF fa =
  case runExcept fa of
    Left err ->
      throwException (error $ intercalate ", " $ renderForeignError <$> err)
    Right a ->
      pure a

-- | Run the F in Aff
affF :: forall eff a. F a -> Aff eff a
affF fa =
  liftEff' $ runF fa

-- | Get the global javascript Window object
window :: Window
window =
  primitives.window

-- | Get the Window's Document
document :: Window -> F Document
document (Window w) =
  w ! "document" >>=
  failNullOrUndefined "element document" >>=
  pure <<< wrap


-- | Get the element identified by id.
elementById :: ElementId -> Document -> F Element
elementById id doc =
  runFn2 primitives.elementById id doc #
  failNullOrUndefined ("elementById #" <> unwrap id) >>=
  pure <<< wrap


-- | Append the child
-- |
-- | The container/parent is the second argument!
-- | Returns the child element (now inside the parent)
appendChild :: Element -> Element -> F Element
appendChild child parent = do
  let _ = runFn2 primitives.appendChild child parent
  pure child

-- | Clear the element.
-- |
-- | Removes all child nodes of the element.
-- | Returns the element.
clearElement :: Element -> F Element
clearElement elem = do
  let _ = runFn1 primitives.clearElement elem
  pure elem

-- | Focus the element.
-- |
-- | Returns the element for easy chaining.
focusElement :: Element -> F Element
focusElement elem = do
  let _ = runFn1 primitives.focusElement elem
  pure elem


-- | Get the first element that matches the selector
-- |
-- | The second argument can be a document or element.
querySelector :: String -> Foreign -> F Element
querySelector str docOrElem =
  runFn2 primitives.querySelector str docOrElem #
  failNullOrUndefined ("querySelector " <> str) >>=
  pure <<< wrap

-- | Get all elements matching the selector.
-- |
-- | If the second argument is an Element,
-- | only descendants will be returned.
querySelectorAll :: String -> Foreign -> F (Array Element)
querySelectorAll str docOrElem = do
  pure $ runFn2 primitives.querySelectorAll str docOrElem

-- | Select the (input) element's text.
-- |
-- | Returns the element for easy chaining.
selectElement :: Element -> F Element
selectElement elem = do
  let _ = runFn1 primitives.selectElement elem
  pure elem

-- | Get the elements text content.
textContent :: Element -> F String
textContent (Element elem) =
  elem ! "textContent" >>= readString


-- | The elements inner HTML property
innerHTML :: Element -> F String
innerHTML (Element elem) =
  elem ! "innerHTML" >>= readString

-- | The elements document.
ownerDocument :: Element -> F Document
ownerDocument (Element elem) =
  elem ! "ownerDocument" >>=
  failNullOrUndefined "element ownerDocument" >>=
  pure <<< wrap

-- | Request animation frame.
requestAnimationFrame :: forall eff. Eff eff Unit -> Window -> F RequestAnimationFrameId
requestAnimationFrame eff =
  pure <<< runFn2 primitives.requestAnimationFrame eff
