-- | Heaviliy inspirec by purescript-debug
module Bonsai.Debug
  (StartTiming, debugJsonObj, debugTiming, logObj, logJson, logJsonObj, logTiming, startTiming)
where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (Foreign)

-- class DebugWarning
--
-- instance warn :: Warn "Bonsai.Debug usage" => DebugWarning

-- | Log a message and object to the console.
foreign import logObj
  :: forall eff a
  .  String -> a -> Eff (console::CONSOLE|eff) Unit

-- | Log a message and JSON.stringify of object to the console.
foreign import logJson
  :: forall eff a
  .  String -> a -> Eff (console::CONSOLE|eff) Unit


-- | Log a message, JSON.stringify and the object to the console.
foreign import logJsonObj
  :: forall eff a
  .  String -> a -> Eff (console::CONSOLE|eff) Unit

newtype StartTiming =
  StartTiming Foreign

-- | Obtain a start time for timing function execution
foreign import startTiming
  :: forall eff
  .  Eff (console::CONSOLE|eff) StartTiming

-- | Log timing information
foreign import logTiming
  :: forall eff
  .  String -> StartTiming -> Eff (console::CONSOLE|eff) Unit

-- | Log timing if in debug mode
debugTiming
  :: forall eff
  .  Boolean -> String -> StartTiming -> Eff (console::CONSOLE|eff) Unit
debugTiming dbg msg start =
  if dbg
    then logTiming msg start
    else pure unit

-- | Log json obj if in debug mode
debugJsonObj
  :: forall eff a
  .  Boolean -> String -> a -> Eff (console::CONSOLE|eff) Unit
debugJsonObj dbg msg obj =
  if dbg
    then logJsonObj msg obj
    else pure unit
