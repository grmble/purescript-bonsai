-- | Bonsai DOM Helpers
-- |
-- | A program using a virtual DOM does not need
-- | much from the real DOM: finding/clearing
-- | elements for bootstrapping the virtual dom,
-- | and DOM effects that can't be expressed
-- | in the virtual dom, like setting the focus
-- | or selecting an input's text.
-- |
-- | This DOM package uses F for the DOM access.
-- | This is much nicer to work with than some
-- | `Eff eff (Maybe Element)`.  You have to run
-- | the underlying except though, convenience
-- | helpers `effF` and `affF` are provided.
-- |
-- | The package is designed to work
-- | with JSDOM for testing or sever side rendering.
-- | That is why all functions take care to
-- | take a Window or Document argument instead
-- | of using a global.
module Bonsai.DOM
  ( DOM
  , Document(..)
  , Element(..)
  , ElementId(..)
  , Window(..)
  , RequestAnimationFrameId(..)

  , failNullOrUndefined
  , copyFakeArray
  , foreignErrorMsg
  , affF
  , effF

  , appendChild
  , clearElement
  , document
  , elementById
  , focusElement
  , innerHTML
  , location
  , locationHash
  , ownerDocument
  , querySelector
  , querySelectorAll
  , requestAnimationFrame
  , selectElementText
  , setLocationHash
  , textContent
  , window
  )
where

import Prelude

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Foreign (F, Foreign, ForeignError(..), fail, isNull, isUndefined, readString, renderForeignError)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Newtype (class Newtype, unwrap, wrap)

-- | Effect for conversion to Eff/Aff
foreign import data DOM :: Effect


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
  { window :: Foreign
  , elementById :: Fn2 ElementId Document Foreign
  , appendChild :: Fn2 Element Element Unit
  , clearElement :: Fn1 Element Unit
  , copyFakeArray :: Fn1 Foreign (Array Foreign)
  , focusElement :: Fn1 Element Unit
  , selectElement :: Fn1 Element Unit
  , setLocationHash :: Fn2 String Foreign Unit
  , querySelector :: Fn2 String Foreign Foreign
  , querySelectorAll :: Fn2 String Foreign Foreign
  , requestAnimationFrame :: Fn2 (Eff eff Unit) Window RequestAnimationFrameId
  }


-- | Fail the F if the forein value is null or undefined
failNullOrUndefined :: String -> Foreign -> F Foreign
failNullOrUndefined msg x =
  if isNull x || isUndefined x
    then fail $ ForeignError ("null or undefined: " <> msg)
    else pure x

-- | Produce an error message for the Left runExcept result
foreignErrorMsg
  :: forall f
  .  Foldable f => Functor f
  => f ForeignError -> String
foreignErrorMsg err =
  intercalate ", " $ renderForeignError <$> err

-- | Run the F in Eff
-- |
-- | Errors will be thrown as exception
effF :: forall eff a. F a -> Eff (dom::DOM,exception::EXCEPTION|eff) a
effF fa =
  case runExcept fa of
    Left err ->
      throw $ foreignErrorMsg err
    Right a ->
      pure a

-- | Run the F in Aff
affF :: forall eff a. F a -> Aff (dom::DOM|eff) a
affF fa =
  liftEff' $ effF fa

-- | Get the global javascript Window object
window :: F Window
window =
  primitives.window #
  failNullOrUndefined "global window" >>=
  pure <<< Window

-- | Get the Window's Document
document :: Window -> F Document
document (Window w) =
  w ! "document" >>=
  failNullOrUndefined "element document" >>=
  pure <<< wrap

-- | Copy an array-ish object to a real array.
-- |
-- | The foreign object needs to have a length property
-- | and properties for all the indices [0, length)
copyFakeArray :: Foreign -> F (Array Foreign)
copyFakeArray =
  pure <<< primitives.copyFakeArray

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

-- | Get the documents window
defaultView :: Document -> F Window
defaultView (Document doc) =
  doc ! "defaultView" >>=
  failNullOrUndefined "defaultView" >>=
  pure <<< wrap

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

-- | The current Location object
-- |
-- | The javascript version also takes a Window,
-- | this only provides the Document version because
-- | Bonsai Tasks have Documents in their context.
location :: Document -> F Foreign
location (Document doc) =
  doc ! "location"

-- | The current location hash
-- |
-- | This is a # followed by the fragment of the URL
-- | displayed in the browsers navigation bar.
locationHash :: Document -> F String
locationHash doc = do
  loc <- location doc
  loc ! "hash" >>= readString

-- | Set the location fragment.
-- |
-- | The string should start with an `#`
setLocationHash :: String -> Document -> F Unit
setLocationHash str doc = do
  loc <- location doc
  pure $ runFn2 primitives.setLocationHash str loc

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
querySelectorAll str docOrElem =
  runFn2 primitives.querySelectorAll str docOrElem #
  failNullOrUndefined "querySelectorAll" >>=
  copyFakeArray >>=
  pure <<< map Element

-- | Select the (input) element's text.
-- |
-- | Returns the element for easy chaining.
selectElementText :: Element -> F Element
selectElementText elem = do
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
