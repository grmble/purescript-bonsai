-- | Bonsai HTML Event helpers
-- |
-- | These are convenience functions, if you use an event handler
-- | A LOT, you should call on with a top level function or memoized function.
-- | See explanation in Bonsai.EventHandlers
module Bonsai.Html.Events
  ( module Bonsai.VirtualDom
  , preventDefaultStopPropagation
  , onClick
  , onClickPreventDefault
  , onCheckedChange
  , onInput
  , onKeyEnter
  , onKeyEnterEscape
  , onSubmit
  )
where

import Prelude

import Bonsai.EventHandlers (constHandler, enterEscapeKeyHandler, enterKeyHandler, targetCheckedHandler, targetValueHandler)
import Bonsai.VirtualDom (Property, Options, on, onWithOptions)


-- | Event options: prevent default, stop propagation
preventDefaultStopPropagation :: Options
preventDefaultStopPropagation =
  { preventDefault: true
  , stopPropagation: true
  }


-- | Suboptimal helper for the input event.
onInput :: forall msg. (String -> msg) -> Property msg
onInput fn =
  on "input" (targetValueHandler fn)


-- | Suboptimal helper for boolean (checkbox) input
onCheckedChange :: forall msg. (Boolean -> msg) -> Property msg
onCheckedChange fn =
  on "change" (targetCheckedHandler fn)


-- | Suboptimal click event handler.
onClick :: forall msg. msg -> Property msg
onClick msg =
  on "click" (const $ pure $ pure msg)


-- | Suboptimal click handler that does not bubble.
-- |
-- | Use it with links.
onClickPreventDefault :: forall msg. msg -> Property msg
onClickPreventDefault msg =
  onWithOptions preventDefaultStopPropagation "click" (constHandler msg)


-- | Emit commands on enter key presses
onKeyEnter :: forall msg. (String -> msg) -> Property msg
onKeyEnter fn =
  on "keydown" (enterKeyHandler fn)


-- | Emit commands on enter or escape key presses
onKeyEnterEscape :: forall msg. (String -> msg) -> (String -> msg) -> Property msg
onKeyEnterEscape enterFn escFn =
  on "keydown" (enterEscapeKeyHandler enterFn escFn)


-- | Emit a command on form submit.
onSubmit :: forall msg. msg -> Property msg
onSubmit msg =
  onWithOptions preventDefaultStopPropagation "submit" (constHandler msg)
