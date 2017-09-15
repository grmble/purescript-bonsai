-- | Bonsai Types
module Bonsai.Types
  ( BrowserEvent
  , Cmd(..)
  , EventDecoder
  , CmdDecoder
  , Emitter
  , f2cmd
  , emptyCommand
  , pureCommand
  , errCommand
  , laterCommand
  )
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, renderForeignError)


-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | * NoCmd means an empty message.
-- | * ErrCmd encodes an error.  Mainly so there is a central place
-- |   for logging errors that may come from the event decoding stage
-- |   or from errors in an asynchronous command.
-- | * Cmd carries a pure message that should be applied.
-- | * Later will produce a Cmd or ErrCmd some time in the future
data Cmd eff msg
  = NoCmd
  | ErrCmd Error
  | Cmd msg
  | Later (Aff eff msg)

-- Cmd is a functor so VNodes/Events can be mapped
instance cmdFunctor :: Functor (Cmd eff) where
  map f NoCmd = NoCmd
  map _ (ErrCmd s) = (ErrCmd s)
  map f (Cmd msg) = Cmd $ f msg
  map f (Later aff) = Later $ map f aff


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

-- | Emitters will get the Cmd into the Bonsai event loop.
-- |
-- | The javascript side has little knowlege about
-- | the internal structure of the purescript types,
-- | so an emitting function is provided that takes
-- | care of all that.
type Emitter aff msg
  =  Cmd aff msg
  -> Eff aff Unit


-- | Helper to turn an F into a Cmd
f2cmd
  :: forall a b eff
  .  (a -> Cmd eff b)
  -> F a
  -> Cmd eff b
f2cmd cmdFn fa =
  case runExcept fa of
    Left errs ->
      ErrCmd $ error $ intercalate ", " $ renderForeignError <$> errs
    Right a ->
      cmdFn a

-- | Produces an empty command.
emptyCommand :: forall aff msg. Cmd aff msg
emptyCommand = NoCmd

-- | Produces a pure command
pureCommand :: forall aff msg. msg -> Cmd aff msg
pureCommand = Cmd

-- | Procuces an error command
errCommand :: forall aff msg. Error -> Cmd aff msg
errCommand = ErrCmd

-- | Produce an asynchronous command
laterCommand :: forall aff msg. Aff aff msg -> Cmd aff msg
laterCommand = Later
