-- | Bonsai HTML Event helpers
-- |
-- | These are convenience functions, if you use an event handler
-- | A LOT, you should call on with a top level function.
-- | See explanation in Bonsai.EventDecoder
module Bonsai.Html.Events
  ( module Bonsai.VirtualDom
  , preventDefaultStopPropagation
  , onClick
  , onInput
  , onKeyEnter
  , onKeyEnterEscape
  , onSubmit
  )
where

import Prelude

import Bonsai.EventDecoder (enterEscapeKeyEvent, targetValueEvent, targetValuesEvent)
import Bonsai.Types (emptyCommand, f2cmd, pureCommand)
import Bonsai.VirtualDom (Property, Options, on)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)


-- | Event options: prevent default, stop propagation
preventDefaultStopPropagation :: Options
preventDefaultStopPropagation =
  { preventDefault: true
  , stopPropagation: true
  }

-- | Suboptimal helper for the input event.
onInput :: forall msg. (String -> msg) -> Property msg
onInput f =
  on "input" (f2cmd pureCommand <<< map f <<< targetValueEvent)

-- | Event listener property for the click event.
onClick :: forall msg. msg -> Property msg
onClick msg =
  on "click" \_ -> Right $ pureCommand msg


-- | Emit commands on enter key presses
onKeyEnter :: forall msg. (String -> msg) -> Property msg
onKeyEnter cmdFn =
  on "keydown" (f2cmd convert <<< enterEscapeKeyEvent)
  where
    convert Nothing =
      emptyCommand
    convert (Just (Left _)) =
      emptyCommand
    convert (Just (Right s)) =
      pureCommand $ cmdFn s

-- | Emit commands on enter or escape key presses
onKeyEnterEscape :: forall msg. (String -> msg) -> (String -> msg) -> Property msg
onKeyEnterEscape enterFn escFn =
  on "keydown" (f2cmd convert <<< enterEscapeKeyEvent)
  where
    convert Nothing =
      emptyCommand
    convert (Just (Left s)) =
      pureCommand $ escFn s
    convert (Just (Right s)) =
      pureCommand $ enterFn s

-- | Emit a command on form submit.
onSubmit :: forall msg. (StrMap String -> msg) -> Property msg
onSubmit cmdFn =
  on "submit" (f2cmd (pureCommand <<< cmdFn) <<< targetValuesEvent)
