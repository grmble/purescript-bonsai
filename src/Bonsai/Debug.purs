-- | Heaviliy inspired by purescript-debug
-- |
-- | This contains helper functions to log arbitrary
-- | objects to the javscript console.  The bad news
-- | is that logging purescript objects from javascript
-- | is not very readable.
-- |
-- | It does have the advantage that we don't require
-- | a Show instance for everyting.s
module Bonsai.Debug
  ( StartTiming
  , debugJsonObj
  , debugTiming
  , logObj
  , logJson
  , logJsonObj
  , logTiming
  , startTiming)
where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

-- class DebugWarning
--
-- instance warn :: Warn "Bonsai.Debug usage" => DebugWarning

-- | Log a message and object to the console.
foreign import logObj
  :: forall a
  .  String -> a -> Effect Unit

-- | Log a message and JSON.stringify of object to the console.
foreign import logJson
  :: forall a
  .  String -> a -> Effect Unit


-- | Log a message, JSON.stringify and the object to the console.
foreign import logJsonObj
  :: forall a
  .  String -> a -> Effect Unit

newtype StartTiming =
  StartTiming Foreign

-- | Obtain a start time for timing function execution
foreign import startTiming :: Effect StartTiming

-- | Log timing information
foreign import logTiming :: String -> StartTiming -> Effect Unit

-- | Log timing if in debug mode
debugTiming :: Boolean -> String -> StartTiming -> Effect Unit
debugTiming dbg msg start =
  if dbg
    then logTiming msg start
    else pure unit

-- | Log json obj if in debug mode
debugJsonObj :: forall a. Boolean -> String -> a -> Effect Unit
debugJsonObj dbg msg obj =
  if dbg
    then logJsonObj msg obj
    else pure unit
