module Bonsai where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef)
import Control.Monad.Reader (ReaderT, runReaderT, asks, ask)
import Control.Monad.State (State, StateT, execState, execStateT, get, gets, modify, put)
import Data.Array (null)
import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId, documentToNonElementParentNode, elementToNode)
import Native.VirtualDom (VNode, render, diff, applyPatches)


-- | A Command represents messages that should be applied to the model
newtype Cmd msg =
  Cmd (Array msg)

-- | Env describes the Bonsai program.
-- |
-- | It is passed around in a ReaderT and stores
-- | callbacks and a ref to the pending commands.
-- | Event callbacks append to the list of pending
-- | commands, they will then be applied
-- | in the main event loop.
type Env model msg =
  { updater  :: model -> msg -> model
  , renderer :: model -> VNode msg
  , pending  :: Ref (Array msg)
  }

-- | ProgramState tracks the current state of the model, vnode and
-- | dom element.
-- |
-- | These are needed to advance the state in reaction to a Cmd message.
type ProgramState model msg =
  { model :: model
  , vnode :: VNode msg
  , dnode :: Element
  }

-- | Create initial environment for the Bonsai program.
programEnv
  :: forall eff model msg
  .  (model -> msg -> model)
  -> (model -> VNode msg)
  -> Eff (ref::REF|eff) (Env model msg)
programEnv updater renderer = do
  ref <- newRef []
  pure { updater: updater, renderer: renderer, pending: ref }



programState
  :: forall eff model msg
  .  Element
  -> Env model msg
  -> model
  -> Eff (dom::DOM|eff) (ProgramState model msg)
programState container env model = do
  let vnode = env.renderer model
  let dnode = render vnode
  _ <- appendChild (elementToNode dnode) (elementToNode container)
  pure $ { model:model, vnode: vnode, dnode:dnode }

-- | Queue a command that will be applied to the model.
queueCommand
  :: forall eff model msg
  .  Env model msg
  -> Cmd msg
  -> Eff (ref::REF|eff) Unit
queueCommand env cmd = do
  modifyRef env.pending $ appendCmd cmd
  pure unit
  where
    appendCmd (Cmd cmd) pending = pending <> cmd

type Program eff model msg a =
  -- StateT (ProgramState model msg) (ReaderT (Env model msg) (Eff (ref::REF|eff))) a
  ReaderT (Env model msg) (StateT (ProgramState model msg) (Eff (dom::DOM,ref::REF|eff))) a


hasPending :: forall eff model msg. Program eff model msg Boolean
hasPending = do
  ref <- asks _.pending
  ps <- liftEff $ readRef ref
  pure $ null ps

step :: forall eff model msg. Program eff model msg Unit
step = do
  cfg <- ask
  state <- get
  msgs <- liftEff $ modifyRef' cfg.pending $ \ms -> {state: [], value: ms}

  let model2 = foldl cfg.updater state.model msgs
  let vnode2 = cfg.renderer model2
  let patch = diff state.vnode vnode2

  dnode2 <- liftEff $ applyPatches state.dnode state.vnode patch

  put {model: model2, vnode: vnode2, dnode: dnode2}


execProgram
  :: forall eff model msg a
  .  Program eff model msg a
  -> Env model msg
  -> ProgramState model msg
  -> Eff (ref::REF,dom::DOM|eff) (ProgramState model msg)
execProgram prog env ps =
  execStateT (runReaderT prog env) ps


-- | Gets a DOM Element by its ID
domElementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe Element)
domElementById id =
  window >>=
  document >>=
  htmlDocumentToDocument >>> documentToNonElementParentNode >>> getElementById id
