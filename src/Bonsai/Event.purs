-- |  Event listener utilities
-- |
-- |  For maximum performance, event handlers attached to the
-- |  virtual DOM should be comparable by object identity.
-- |  For this, you have call on with a top level function
-- |  event decoder.
-- |
-- |  So for top VDOM diff/patching performance, instead of
-- |  using onInput, you would make a top level function
-- |
-- |    decodeXxx event = eventDecoder Xxx targetValue
-- |
-- |    -- use it like
-- |    on "input" decodeXxx
-- |
-- |  Or you might just have a single event handler on a
-- |  parent element and look at the event target.

module Bonsai.Event
  ( onInput
  , onInputWithOptions
  , onClick
  , onClickWithOptions
  , onEnter
  , onSubmit
  , preventDefaultStopPropagation
  , targetValue
  , targetFormValues
  , targetValues
  , ignoreEscape
  )
where

import Prelude


import Bonsai.VirtualDom (Options, Property, EventDecoder, on, onWithOptions)
import Control.Plus (empty)
import Data.Array (range, catMaybes)
import Data.Foreign (F, Foreign, ForeignError(..), isNull, isUndefined, readInt, readString, fail)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- | Event listener property for the input event.
-- |
-- | Should be defined on an input. Will call
-- | the message constructor with the current value
-- | of the input element.
onInput :: Property String
onInput =
  on "input" targetValue

onInputWithOptions :: Options -> Property String
onInputWithOptions options =
  onWithOptions "input" options targetValue

-- | Event listener property for the click event.
onClick :: forall msg. msg -> Property msg
onClick msg =
  on "click" (const $ pure $ pure msg)

onClickWithOptions :: forall msg. Options -> msg -> Property msg
onClickWithOptions options msg =
  onWithOptions "click" options (const $ pure $ pure msg)

-- | Event listener property for the submit event.
-- |
-- | Should be defined on the form. Will prevent default
-- | and stop propagation and will call the constructor
-- | with a map of the current form values.
onSubmit :: Property (StrMap String)
onSubmit =
  onWithOptions "submit" preventDefaultStopPropagation targetValues

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

-- | Read the value of the target input element
targetValue :: EventDecoder String
targetValue value =
  pure <$> (value ! "target" ! "value" >>= readString)

-- ! Read the names and values of the target element's form.
targetFormValues :: EventDecoder (StrMap String)
targetFormValues value =
  value ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValues :: EventDecoder (StrMap String)
targetValues value =
  value ! "target" >>= namesAndValues

-- | Read a strmap of values from a (fake) array of input elements
namesAndValues :: EventDecoder (StrMap String)
namesAndValues arr = do
  len <- arr ! "length" >>= readInt
  (pure <<< fromFoldable <<< catMaybes) <$> traverse (nameAndValue arr) (range 0 (len - 1))

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
ignoreEscape :: EventDecoder Unit
ignoreEscape event = do
  keyCode <- event ! "keyCode" >>= readInt
  if keyCode == 27 -- ESC
    then pure $ pure unit
    else fail (ForeignError "there is no escape")
