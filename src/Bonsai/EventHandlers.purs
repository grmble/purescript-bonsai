-- |  Event Handlers
-- |
-- |  For maximum performance, event handlers attached to the
-- |  virtual DOM should be comparable by object identity.
-- |  For this, you have call `on` with a top level function.
-- |
-- |
-- |  This module defines composable event decoding helpers
-- |  and convience event handling functions.
-- |  For maximum performance,
-- |  you should define your own toplevel helpers.
-- |  This module aims to provide helpers that faciliate
-- |  writing performant event handlers.
-- |
-- |  Bonsai.Html.Events aims to provide convenient helpers.
-- |
-- |
-- |  Example:
-- |
-- |    -- convenient but not optimal for handlers that get used a lot
-- |    -- onInput is from Bonsai.Html.Events
-- |    onInput MyMsg
-- |
-- |    -- good because the virtual can leave the event handler alone
-- |    on "input" onTargetValueMyMsg
-- |    onTargetValueMyMsg = targetValueHandler MyMsg
-- |
module Bonsai.EventHandlers
  ( module Bonsai.VirtualDom
  , constHandler
  , dataAttribute
  , dataAttributeHandler
  , enterEscapeKey
  , enterEscapeKeyHandler
  , enterKeyHandler
  , keyCode
  , namesAndValues
  , targetChecked
  , targetCheckedHandler
  , targetFormValues
  , targetValue
  , targetValueHandler
  , targetValues
  )
where

import Prelude

import Bonsai (Cmd)
import Bonsai.DOM (copyFakeArray)
import Bonsai.VirtualDom (Options, Property, on, onWithOptions, defaultOptions)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, isNull, isUndefined, readBoolean, readInt, readNullOrUndefined, readString)
import Data.Foreign.Index ((!))
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)


-- | Event handler that always sends the same command
-- |
-- | For use with `on "click"`
constHandler :: forall eff msg. msg -> Foreign -> F (Cmd eff msg)
constHandler msg =
  const $ pure $ pure msg


-- | Read the value of the target input element
targetValue :: Foreign -> F String
targetValue event =
  event ! "target" ! "value" >>= readString

-- | Event handler for target value.
-- |
-- | `on "input" (targetValueHandler MyMsg)`
targetValueHandler :: forall eff msg. (String -> msg) -> Foreign -> F (Cmd eff msg)
targetValueHandler fn ev =
  map (pure <<< fn) (targetValue ev)


-- | Read the value of target element's checked property
targetChecked :: Foreign -> F Boolean
targetChecked event =
  event ! "target" ! "checked" >>= readBoolean


-- | Event handler for target's checked property.
targetCheckedHandler :: forall eff msg. (Boolean -> msg) -> Foreign -> F (Cmd eff msg)
targetCheckedHandler fn ev =
  map (pure <<< fn) (targetChecked ev)


-- ! Read the names and values of the target element's form.
targetFormValues :: Foreign -> F (Map String (NEL.NonEmptyList String))
targetFormValues event =
  event ! "target" ! "form" >>= namesAndValues


-- | Read the names and values of target form, for form events.
targetValues :: Foreign -> F (Map String (NEL.NonEmptyList String))
targetValues event =
  event ! "target" >>= namesAndValues


-- | Read the events keyCode
keyCode :: Foreign -> F Int
keyCode event =
  event ! "keyCode" >>= readInt


-- | key event code helper: Right for ENTER, Left for ESC
enterEscapeKey :: Foreign -> F (Maybe (Either String String))
enterEscapeKey event = do
  kc    <- keyCode event
  value <- targetValue event
  case kc of
    13 -> -- ENTER
      pure (Just (Right value))
    27 -> -- ESCAPE
      pure (Just (Left value))
    _ ->
      pure Nothing


-- | Event handler for enter key.
enterKeyHandler :: forall eff msg. (String -> msg) -> Foreign -> F (Cmd eff msg)
enterKeyHandler fn ev =
  map convert (enterEscapeKey ev)
  where
    convert Nothing =
      empty
    convert (Just (Left _)) =
      empty
    convert (Just (Right s)) =
      pure $ fn s


-- | Event handler for enter and escape keys.
enterEscapeKeyHandler :: forall eff msg. (String -> msg) -> (String -> msg) -> Foreign -> F (Cmd eff msg)
enterEscapeKeyHandler enterFn escFn ev =
  map convert (enterEscapeKey ev)
  where
    convert Nothing =
      empty
    convert (Just (Left s)) =
      pure $ escFn s
    convert (Just (Right s)) =
      pure $ enterFn s


-- | Read names and values from a (fake) foreign array.
-- |
-- | This is meant to be used on an array of dom nodes.
namesAndValues :: Foreign -> F (Map String (NEL.NonEmptyList String))
namesAndValues fakeArr = do
  lst <- List.fromFoldable <$> copyFakeArray fakeArr
  (fromFoldable <<< groupByName <<< List.catMaybes) <$> traverse nameAndValue lst

  where
    groupByName :: List.List (Tuple String String) -> List.List (Tuple String (NEL.NonEmptyList String))
    groupByName tups =
      map toTup grouped

      where
        grouped = List.groupBy (\a b -> fst a == fst b) tups

        toTup ne =
          Tuple k vs
          where
            k = fst $ NEL.head ne
            vs = map snd ne


nameAndValue :: Foreign -> F (Maybe (Tuple String String))
nameAndValue elem = do
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


-- | Read the events data attribute by name.
-- |
-- | Tries to read recursively from parent elements.
dataAttribute :: String -> Foreign -> F String
dataAttribute name event = do
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

-- | Event handler extracting a data attribute.
-- |
-- | It should be possible to memoize this on the attribute name.
dataAttributeHandler :: forall eff msg. (String -> msg) -> String -> Foreign -> F (Cmd eff msg)
dataAttributeHandler fn name ev =
  map (pure <<< fn) (dataAttribute name ev)
