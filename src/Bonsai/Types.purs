-- | This module defines the central Bonsai types
-- | that are used in the Core and VirtualDom modules.
module Bonsai.Types
  ( BONSAI
  , Cmd(..)
  , Document(..)
  , Element(..)
  , TaskContext
  , Window(..)
  , emitMessage
  , emittingTask
  , emptyCommand
  , pureCommand
  , simpleTask
  , unitTask
  )
where

import Prelude

import Control.Monad.Aff (Aff, Fiber)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Foldable (for_)
import Data.Foreign (Foreign)
import Data.Monoid (class Monoid)


-- | Effect for public types
foreign import data BONSAI :: Effect

-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | A command is either Pure (the constructor Cmd)
-- | or an asynchronous Task.  The pure command simply contains
-- | the messages that will be emitted.
-- | The asynchronous Task gets an emitter function
-- | that it can use to emit messages at will.
data Cmd eff msg
  = Cmd (Array msg)
  | TaskCmd (TaskContext eff msg -> Aff eff Unit)

-- Cmd is a functor so VNodes/Events can be mapped
instance cmdFunctor :: Functor (Cmd eff) where
  map f (Cmd ms) =
    Cmd $ map f ms
  map f (TaskCmd task) =
    TaskCmd $ mapTask f task

mapTask
  :: forall eff a b
  .  (a -> b)
  -> (TaskContext eff a -> Aff eff Unit)
  -> (TaskContext eff b -> Aff eff Unit)
mapTask f ta contextB =
  let
    emitA :: a -> Eff eff Unit
    emitA as =
      contextB.emitter $ f as
    contextA :: TaskContext eff a
    contextA =
      contextB { emitter = emitA }
  in
    ta contextA

-- | Emit helper for Tasks.
-- |
-- | In an emitting task, use this function to emit messages.
emitMessage :: forall aff msg. TaskContext aff msg -> msg -> Aff aff Unit
emitMessage ctx msg =
  unsafeCoerceAff $ liftEff $ ctx.emitter msg


-- | Produces a simple task (not cancellable, ony emits the return values
simpleTask :: forall aff msg. Aff aff msg -> Cmd aff msg
simpleTask aff =
  TaskCmd $ \ctx ->
    aff >>= emitMessage ctx

-- | Procudes a task that can emit multiple times
emittingTask
  :: forall aff msg
  .  (TaskContext aff msg -> Aff aff Unit)
  -> Cmd aff msg
emittingTask = TaskCmd


-- | An effectful task without return value - e.g. write to storage, ...
unitTask :: forall aff msg. Aff aff Unit -> Cmd aff msg
unitTask aff =
  TaskCmd $ \_ -> aff



-- | Semigroup instance for Cmd
-- |
-- | aside from the obvious (combining commands with <>)
-- | this will also make (Tuple (Cmd eff) Model)
-- | an applicative functor (= the results of update functions)
instance semigroupCmd :: Semigroup (Cmd eff msg) where
  append (Cmd m1) (Cmd m2) =
    Cmd (m1 <> m2)
  append (Cmd m) (TaskCmd task) =
    TaskCmd \ctx -> do
      for_ m (emitMessage ctx)
      task ctx
  append (TaskCmd task) (Cmd m) =
    TaskCmd \ctx -> do
      task ctx
      for_ m (emitMessage ctx)
  append (TaskCmd t1) (TaskCmd t2) =
    TaskCmd \ctx -> do
      t1 ctx
      t2 ctx


instance monoidCmd :: Monoid (Cmd eff msg) where
  mempty = Cmd []


-- | The type for the global javascript document
newtype Document =
  Document Foreign

-- | The type for the global javascript window
newtype Window =
  Window Foreign

-- | The type for a dom element
newtype Element =
  Element Foreign

-- | The Task Context holds the emitter function for the task
-- |
-- | Maybe: a way to get at the current model?
-- | That would mean the model has to encoded in all the view functions
-- | as well.  Maybe not a good idea.
type TaskContext eff msg =
  { emitter :: msg -> Eff eff Unit
  , fiber :: AVar (Fiber eff Unit)
  , document :: Document
  }

-- | Produces an empty command.
emptyCommand :: forall aff msg. Cmd aff msg
emptyCommand = Cmd []

-- | Produces a pure command
pureCommand :: forall aff msg. msg -> Cmd aff msg
pureCommand msg = Cmd [msg]
