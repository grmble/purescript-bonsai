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

import Bonsai.VirtualDom (Options, Property, on, onWithOptions, defaultOptions)
import Data.List (List, catMaybes, groupBy, range)
import Data.List.NonEmpty as NEL
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), isNull, isUndefined, readBoolean, readInt, readNullOrUndefined, readString, fail)
import Data.Foreign.Index ((!))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)

-- | The simplest possible browser event - the foreign event itself
identityEvent :: Foreign -> F Foreign
identityEvent =
  pure

-- | Read the value of the target input element
targetValueEvent :: Foreign -> F String
targetValueEvent event =
  event ! "target" ! "value" >>= readString

-- | Read the value of target element's checked property
targetCheckedEvent :: Foreign -> F Boolean
targetCheckedEvent event =
  event ! "target" ! "checked" >>= readBoolean

-- ! Read the names and values of the target element's form.
targetFormValuesEvent :: Foreign -> F (Map String (NEL.NonEmptyList String))
targetFormValuesEvent event =
  event ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValuesEvent :: Foreign -> F (Map String (NEL.NonEmptyList String))
targetValuesEvent event =
  event ! "target" >>= namesAndValues

keyCodeEvent :: Foreign -> F Int
keyCodeEvent event =
  event ! "keyCode" >>= readInt

-- | Event decoding helper: Right for ENTER, Left for ESC
enterEscapeKeyEvent :: Foreign -> F (Maybe (Either String String))
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
namesAndValues :: Foreign -> F (Map String (NEL.NonEmptyList String))
namesAndValues arr = do
  len <- arr ! "length" >>= readInt
  (fromFoldable <<< groupByName <<< catMaybes) <$> traverse (nameAndValue arr) (range 0 (len - 1))

  where
    groupByName :: List (Tuple String String) -> List (Tuple String (NEL.NonEmptyList String))
    groupByName tups =
      map toTup grouped

      where
        grouped = groupBy (\a b -> fst a == fst b) tups

        toTup ne =
          Tuple k vs
          where
            k = fst $ NEL.head ne
            vs = map snd ne


nameAndValue :: Foreign -> Int -> F (Maybe (Tuple String String))
nameAndValue arr idx = do
  elem <- arr ! idx
  name <- elem ! "name" >>= rnu readString
  value <- elem ! "value" >>= rnu readString
  checked <- elem ! "checked" >>= rnu readBoolean
  typ <- elem ! "type" >>= rnu readString

  pure $
    case typ of
      Just "checkbox" ->
        handleChecked checked name value
      Just "radio" ->
        handleChecked checked name value
      _ ->
        Tuple <$> name <*> value

  where
    rnu :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
    rnu f a = readNullOrUndefined a >>= traverse f

    handleChecked checked name value = do
      b <- checked
      if b
        then Tuple <$> name <*> value
        else Nothing

isNullOrUndefined :: Foreign -> Boolean
isNullOrUndefined value =
  (isNull value) || (isUndefined value)

-- | Event decoder returns unit or fails
-- |
-- | hack or no hack?
ignoreEscapeEvent :: Foreign -> F Unit
ignoreEscapeEvent event = do
  keyCode <- event ! "keyCode" >>= readInt
  if keyCode == 27 -- ESC
    then pure unit
    else fail (ForeignError "there is no escape")


-- | Event decoder decodes the value of a data attribute
dataAttributeEvent :: String -> Foreign -> F String
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
