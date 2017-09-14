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
  -- , onEnter
  , preventDefaultStopPropagation
  , targetValueEvent
  , targetFormValuesEvent
  , targetValuesEvent
  , ignoreEscapeEvent
  )
where

import Prelude


import Bonsai.Types (BrowserEvent, EventDecoder, f2aff, pureCommand)
import Bonsai.VirtualDom (Options, Property, on, onWithOptions, defaultOptions)
import Data.Array (catMaybes, range)
import Data.Foreign (F, Foreign, ForeignError(..), isNull, isUndefined, readInt, readString, fail)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- | Suboptimal helper for the input event.
onInput :: forall msg. (String -> msg) -> Property msg
onInput f =
  on "input" (pure <<< map f <<< f2aff <<< targetValueEvent)

-- | Event listener property for the click event.
onClick :: forall msg. msg -> Property msg
onClick msg =
  on "click" \_ -> pureCommand msg



-- XXX: cant express this as Array (Aff aff msg)
--      maybe Aff aff (Maybe msg) ???
--
-- -- | Emit commands on enter key presses
-- onEnter :: forall msg. msg -> Property msg
-- onEnter enter =
--   on "enter" \event ->
--     let
--       boolArr = [f2aff (isEnterEvent event) ]
--     in
--       map
--         (const (pure enter))
--         (filter id boolArr)



preventDefaultStopPropagation :: Options
preventDefaultStopPropagation =
  { preventDefault: true
  , stopPropagation: true
  }

-- | The simplest possible browser event - the foreign event itself
identityEvent :: EventDecoder Foreign
identityEvent =
  pure

-- | Read the value of the target input element
targetValueEvent :: EventDecoder String
targetValueEvent event =
  event ! "target" ! "value" >>= readString

-- ! Read the names and values of the target element's form.
targetFormValuesEvent :: EventDecoder (StrMap String)
targetFormValuesEvent event =
  event ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValuesEvent :: EventDecoder (StrMap String)
targetValuesEvent event =
  event ! "target" >>= namesAndValues

keyCodeEvent :: EventDecoder Int
keyCodeEvent event =
  event ! "keyCode" >>= readInt

isEnterEvent :: EventDecoder Boolean
isEnterEvent event =
  map (\kc -> kc == 13) (keyCodeEvent event)

-- | Read names and values from a (fake) foreign array.
-- |
-- | This is meant to be used on an array of dom nodes.
namesAndValues :: EventDecoder (StrMap String)
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
