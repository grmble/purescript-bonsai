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
  , nowCommand
  , laterCommand
  , delayCommand
  )
where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate, singleton)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, renderForeignError)
import Data.Time.Duration (Milliseconds)


-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | There is no monoid instance and no apply because
-- | combining commands would be easy, but doing so
-- | in a performant way would be hard.
-- |
-- | The problem is that Later commands can not be batched,
-- | as soon as a Later would show up in a big list,
-- | you'd have a render for every command in the list.
-- |
-- | So this has to be done manually by the user:
-- | If Pure is not powerful enough, switch to Now.
-- | If Now does not cut it, use Later.  If you need
-- | one command now and one later, you have to
-- | arrange for the update of the immediate message
-- | to trigger the second command.
data Cmd eff msg
  = Pure (Array msg)
  | Now (Eff eff (Array msg))
  | Later (Aff eff (Array msg))

-- Cmd is a functor so VNodes/Events can be mapped
instance cmdFunctor :: Functor (Cmd eff) where
  map f (Pure ms) = Pure $ map f ms
  map f (Now eff) = Now $ map (map f) eff
  map f (Later aff) = Later $ map (map f) aff


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
  (Foreign -> Either Error (Cmd aff msg))

-- | Emitters will get the Cmd into the Bonsai event loop.
-- |
-- | The javascript side has little knowlege about
-- | the internal structure of the purescript types,
-- | so an emitting function is provided that takes
-- | care of all that.
type Emitter aff msg
  =  Either Error (Cmd aff msg)
  -> Eff aff Unit


-- | Helper to turn an F into emittable (Either Error Cmd)
f2cmd
  :: forall a b eff
  .  (a -> Cmd eff b)
  -> F a
  -> Either Error (Cmd eff b)
f2cmd cmdFn fa =
  case runExcept fa of
    Left errs ->
      Left $ error $ intercalate ", " $ renderForeignError <$> errs
    Right a ->
      Right $ cmdFn a

-- | Produces an empty command.
emptyCommand :: forall aff msg. Cmd aff msg
emptyCommand = Pure []

-- | Produces a pure command
pureCommand :: forall aff msg. msg -> Cmd aff msg
pureCommand msg = Pure [msg]

-- | Produces an effectful but synchronous command
nowCommand :: forall aff msg. Eff aff msg -> Cmd aff msg
nowCommand eff = Now $ map singleton eff

-- | Produce an asynchronous command
laterCommand :: forall aff msg. Aff aff msg -> Cmd aff msg
laterCommand aff = Later $ map singleton aff

-- | Produces a command in millis milliseconds
delayCommand :: forall aff msg. Milliseconds -> msg -> Cmd aff msg
delayCommand millis msg = Later $ do
  delay millis
  pure [ msg ]
