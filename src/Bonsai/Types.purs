-- | Bonsai Types
module Bonsai.Types
  ( BrowserEvent
  , Cmd
  , EventDecoder
  , CmdDecoder
  , Emitter
  , f2aff
  , emptyCommand
  , pureCommand
  )
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, renderForeignError)


-- | A Command represents messages that should be applied to the Bonsai model
type Cmd aff msg =
  Array (Aff aff msg)

-- | A BrowserEvent is simply a decoded foreign
-- |
-- | This is inherently composable - it's a full monad.
type BrowserEvent msg = F msg


-- | A EventDecoder decodes a foreign to a BrowserEvent
type EventDecoder msg =
  (Foreign -> BrowserEvent msg)

-- | And finally, a Command Decoder turns a foreign into a command
-- |
-- | The CmdDecoder will be implemented using EventDecoders internally.
type CmdDecoder aff msg =
  (Foreign -> Cmd aff msg)

-- | Emitters are how the Aff callbacks are run.
-- |
-- | The javascript side has little knowlege about
-- | the internal structure of the purescript types,
-- | so an emitting function is provided that takes
-- | care of all that.
type Emitter aff msg
  =  Cmd aff msg
  -> Eff aff Unit


f2aff
  :: forall a eff
  .  F a
  -> Aff eff a
f2aff fa = do
  case runExcept fa of
    Left errs -> throwError $ error $ intercalate ", " $ renderForeignError <$> errs
    Right a -> pure a


-- | Produces an empty command.
emptyCommand :: forall aff msg. Cmd aff msg
emptyCommand = []

-- | Produces a pure command
pureCommand :: forall aff msg. msg -> Cmd aff msg
pureCommand msg = [ pure msg ]
