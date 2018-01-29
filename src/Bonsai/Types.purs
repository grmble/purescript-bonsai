-- | This module defines the central Bonsai types
-- | that are used in the Core and VirtualDom modules.
module Bonsai.Types
  ( BONSAI
  , Cmd(..)
  , Document(..)
  , TaskContext
  , Window(..)
  , emptyCommand
  , pureCommand
  )
where

import Prelude

import Control.Monad.Aff (Aff, Fiber)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff (Eff, kind Effect)
import Data.Foreign (Foreign)


-- | Effect for public types
foreign import data BONSAI :: Effect

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
-- |
-- | There used to be a helper type for TaskContext -> Aff but
-- | for some reason it did not unify in user code.
-- | So the recommendation is to produce the command,
-- | not the function with complicated signature that goes
-- | inside a TaskCmd.
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


-- | The type for the global javascript document
newtype Document =
  Document Foreign

-- | The type for the global javascript window
newtype Window =
  Window Foreign

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
