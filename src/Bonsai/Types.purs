-- | This module defines the central Bonsai types
-- | that are used in the Core and VirtualDom modules.
module Bonsai.Types
  ( BrowserEvent
  , Cmd(..)
  , CmdDecoder
  , Emitter
  , EventDecoder
  , Task
  , TaskContext
  , f2cmd
  , emptyCommand
  , pureCommand
  , simpleTask
  , readerTask
  )
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, renderForeignError)


-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | A command is either Pure (the constructor Cmd)
-- | or an asynchronous Task.  The pure command simply contains
-- | the messages that will be emitted.
-- | The asynchronous Task gets an emitter function
-- | that it can use to emit messages at will.
-- |
-- | There is currently no effectful synchronous command -
-- | there used to be, but it did not turn out very useful
-- | except in dumbed down examples.
-- |
-- | There could be a helper function that expresses simple
-- | effectful commands as Tasks though.
data Cmd eff msg
  = Cmd (Array msg)
  | TaskCmd (Task eff msg)

-- Cmd is a functor so VNodes/Events can be mapped
instance cmdFunctor :: Functor (Cmd eff) where
  map f (Cmd ms) =
    Cmd $ map f ms
  map f (TaskCmd task) =
    TaskCmd $ mapTask f task

-- | A Task is an asynchronous compution that
-- | can emit messages at will via an emitting
-- | functions in its TaskContext.
-- |
-- | It is a newtype because it has unusual needs:
-- | the result type of the Aff is always unit,
-- | so that part may not be mapped.  The emitting
-- | function however needs to be mapped.
type Task eff msg =
  (TaskContext eff (Array msg)) -> (Aff eff (Array msg))

mapTask :: forall eff a b. (a -> b) -> Task eff a -> Task eff b
mapTask f ta contextB =
  let
    emitA :: Array a -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Unit
    emitA as =
      contextB.emitter $ map f as
    contextA :: TaskContext eff (Array a)
    contextA =
      contextB { emitter = emitA }
  in
    map (map f) $ ta contextA

-- | The Task Context holds the emitter function for the task
-- |
-- | Maybe: a way to get at the current model?
-- | That would mean the model has to encoded in all the view functions
-- | as well.  Maybe not a good idea.
type TaskContext eff msg =
  { emitter :: msg -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Unit
  }

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
emptyCommand = Cmd []

-- | Produces a pure command
pureCommand :: forall aff msg. msg -> Cmd aff msg
pureCommand msg = Cmd [msg]


-- | Produces a simple task (not cancellable, ony emits the return values
simpleTask :: forall aff msg. Aff aff (Array msg) -> Cmd aff msg
simpleTask aff = TaskCmd $ \_ -> aff

-- | Procudes a task that can emit multiple times
readerTask :: forall aff msg. Task aff msg -> Cmd aff msg
readerTask = TaskCmd
