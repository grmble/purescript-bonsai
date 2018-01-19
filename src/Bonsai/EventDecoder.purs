-- |  Event decoder utilities
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
-- |    myMsgDecoder = (f2cmd pureCommand <<< map f <<< targetValueEvent)
-- |    onInputMyMsg = on "input" myMsgDecoder
-- |
module Bonsai.EventDecoder
  ( module Bonsai.VirtualDom
  , targetCheckedEvent
  , targetValueEvent
  , targetFormValuesEvent
  , targetValuesEvent
  , keyCodeEvent
  , enterEscapeKeyEvent
  , ignoreEscapeEvent
  , dataAttributeEvent
  )
where

import Prelude

import Bonsai.Types (BrowserEvent, EventDecoder)
import Bonsai.VirtualDom (Options, Property, on, onWithOptions, defaultOptions)
import Data.Array (catMaybes, range)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), isNull, isUndefined, readBoolean, readInt, readNullOrUndefined, readString, fail)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.Map (Map, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | The simplest possible browser event - the foreign event itself
identityEvent :: EventDecoder Foreign
identityEvent =
  pure

-- | Read the value of the target input element
targetValueEvent :: EventDecoder String
targetValueEvent event =
  event ! "target" ! "value" >>= readString

-- | Read the value of target element's checked property
targetCheckedEvent :: EventDecoder Boolean
targetCheckedEvent event =
  event ! "target" ! "checked" >>= readBoolean

-- ! Read the names and values of the target element's form.
targetFormValuesEvent :: EventDecoder (Map String String)
targetFormValuesEvent event =
  event ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValuesEvent :: EventDecoder (Map String String)
targetValuesEvent event =
  event ! "target" >>= namesAndValues

keyCodeEvent :: EventDecoder Int
keyCodeEvent event =
  event ! "keyCode" >>= readInt

-- | Event decoding helper: Right for ENTER, Left for ESC
enterEscapeKeyEvent :: EventDecoder (Maybe (Either String String))
enterEscapeKeyEvent event = do
  kc    <- keyCodeEvent event
  value <- targetValueEvent event
  case kc of
    13 -> -- ENTER
      pure (Just (Right value))
    27 -> -- ESCAPE
      pure (Just (Left value))
    _ ->
      pure Nothing


-- | Read names and values from a (fake) foreign array.
-- |
-- | This is meant to be used on an array of dom nodes.
namesAndValues :: EventDecoder (Map String String)
namesAndValues arr = do
  len <- arr ! "length" >>= readInt
  (fromFoldable <<< catMaybes) <$> traverse (nameAndValue arr) (range 0 (len - 1))

nameAndValue :: Foreign -> Int -> F (Maybe (Tuple String String))
nameAndValue arr idx = do
  elem <- arr ! idx
  name <- elem ! "name" >>= rnu readString
  value <- elem ! "value" >>= rnu readString
  checked <- elem ! "checked" >>= rnu readBoolean
  typ <- elem ! "type" >>= readString

  pure $
    case typ of
      "checkbox" -> do
        b <- checked
        if b
          then Tuple <$> name <*> Just "on"
          else Nothing
      _ ->
        Tuple <$> name <*> value

  where
    rnu :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
    rnu f a = readNullOrUndefined a >>= traverse f

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


-- | Event decoder decodes the value of a data attribute
dataAttributeEvent :: String -> EventDecoder String
dataAttributeEvent name event = do
  target <- event ! "target"
  go target
  where
  go elem = do
    value <- elem ! "dataset" ! name
    if isNullOrUndefined value
      then do
        parent <- elem ! "parentElement"
        go parent
      else readString value
