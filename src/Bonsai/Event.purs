-- |  Event listener utilities
-- |
-- |  For maximum performance, event handlers attached to the
-- |  virtual DOM should be comparable by object identity.
-- |  For this, you have call on with a top level function
-- |  event decoder.
-- |
-- |  This module defines composable BrowserEvents and
-- |  utility functions that use them.  For maximum performance,
-- |  you should define your own toplevel helpers
-- |  with a composed BrowserEvent of your choice.
-- |
-- |
-- |  Example:
-- |
-- |    -- convenient but not optimal for handlers that get used a lot
-- |    onInput MyMsg
-- |    -- good because will compare equal
-- |    myMsgDecoder = decoder (MyMsg <$> targetValue)
-- |    onInputMyMsg = on "input" myMsgDecoder
-- |
module Bonsai.Event
  ( module Bonsai.VirtualDom
  , onInput
  , onClick
  , onEnter
  , onSubmit
  , preventDefaultStopPropagation
  , emptyDecoder
  , constantDecoder
  , decoder
  , targetValueEvent
  , targetFormValuesEvent
  , targetValuesEvent
  , ignoreEscapeEvent
  )
where

import Prelude


import Bonsai.VirtualDom (Options, Property, on, onWithOptions, defaultOptions)
import Bonsai.Types (Cmd, BrowserEvent)
import Control.Plus (empty)
import Data.Array (range, catMaybes)
import Data.Foreign (F, Foreign, ForeignError(..), isNull, isUndefined, readInt, readString, fail)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- | Suboptimal helper for the input event.
onInput :: forall msg. (String -> msg) -> Property msg
onInput f =
  on "input" (decoder (map f <<< targetValueEvent))

-- | Event listener property for the click event.
onClick :: forall msg. msg -> Property msg
onClick msg =
  on "click" (constantDecoder msg)

-- | Event listener property for the submit event.
-- |
-- | Should be defined on the form. Will prevent default
-- | and stop propagation and will call the constructor
-- | with a map of the current form values.
onSubmit :: Property (StrMap String)
onSubmit =
  onWithOptions "submit" preventDefaultStopPropagation (decoder targetValuesEvent)

-- | Emit commands on enter key presses
onEnter :: forall msg. msg -> Property msg
onEnter enter =
  on "keydown" $ \event -> do
    keyCode <- event ! "keyCode" >>= readInt
    case keyCode of
      13 -> -- Enter
        pure $ pure enter
      _ ->
        pure $ empty

preventDefaultStopPropagation :: Options
preventDefaultStopPropagation =
  { preventDefault: true
  , stopPropagation: true
  }

-- | A empty decoder - will only ever emit noop commands
emptyDecoder :: forall msg. Foreign -> F (Cmd msg)
emptyDecoder _ = pure $ empty

-- | A constant decoder - will always produce a constant command
constantDecoder :: forall msg. msg -> Foreign -> F (Cmd msg)
constantDecoder msg _ = pure $ pure msg

-- | Turn a BrowserEvent function into a decoder.
decoder :: forall msg. (Foreign -> BrowserEvent msg) -> Foreign -> F (Cmd msg)
decoder eventFn event =
  pure <$> (eventFn event)

-- | The simplest possible browser event - the foreign event itself
identityEvent :: Foreign -> BrowserEvent Foreign
identityEvent =
  pure

-- | Read the value of the target input element
targetValueEvent :: Foreign -> BrowserEvent String
targetValueEvent event =
  event ! "target" ! "value" >>= readString

-- ! Read the names and values of the target element's form.
targetFormValuesEvent :: Foreign -> BrowserEvent (StrMap String)
targetFormValuesEvent event =
  event ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValuesEvent :: Foreign -> BrowserEvent (StrMap String)
targetValuesEvent event =
  event ! "target" >>= namesAndValues

-- | Read names and values from a (fake) foreign array.
-- |
-- | This is meant to be used on an array of dom nodes.
namesAndValues :: Foreign -> BrowserEvent (StrMap String)
namesAndValues arr = do
  len <- arr ! "length" >>= readInt
  (fromFoldable <<< catMaybes) <$> traverse (nameAndValue arr) (range 0 (len - 1))

nameAndValue :: Foreign -> Int -> F (Maybe (Tuple String String))
nameAndValue arr idx = do
  name <- arr ! idx ! "name"
  value <- arr ! idx ! "value"
  if (isNullOrUndefined name) || (isNullOrUndefined value)
    then pure Nothing
    else do
      n <- readString name
      v <- readString value
      pure (Just (Tuple n v))

isNullOrUndefined :: Foreign -> Boolean
isNullOrUndefined value =
  (isNull value) || (isUndefined value)

-- | Event decoder returns unit or fails
-- |
-- | hack or no hack?
ignoreEscapeEvent :: Foreign -> BrowserEvent Unit
ignoreEscapeEvent event = do
  keyCode <- event ! "keyCode" >>= readInt
  if keyCode == 27 -- ESC
    then pure unit
    else fail (ForeignError "there is no escape")
