module Bonsai
  ( Program
  , ProgramState
  , program
  )
where

import Prelude

import Bonsai.VirtualDom (VNode, Cmd, render, diff, applyPatches)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef, writeRef)
import Data.Foldable (foldl)
import DOM (DOM)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode)


-- | Program describes the Bonsai program.
-- |
-- | It is passed around in a ReaderT and stores
-- | callbacks and a ref to the pending commands.
-- | Event callbacks append to the list of pending
-- | commands, they will then be applied
-- | in the main event loop.
type Program model msg =
  { updater  :: model -> msg -> model
  , renderer :: model -> VNode msg
  , pending  :: Ref (Array msg)
  , state    :: Ref (ProgramState model msg)
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
program
  :: forall eff model msg
  .  Element
  -> (model -> msg -> model)
  -> (model -> VNode msg)
  -> model
  -> Eff (ref::REF,dom::DOM|eff) (Program model msg)
program container updater renderer model = do
  -- use a fake ProgramState so we have a ProgramEnv to render with
  -- (needed for the emitters)
  let vnode = renderer model
  fakeState <- newRef { model: model, vnode: vnode, dnode: container }
  pending   <- newRef []
  let env = { updater: updater, renderer: renderer, pending: pending, state: fakeState }
  let dnode = render (emitter env) vnode
  _ <- appendChild (elementToNode dnode) (elementToNode container)
  modifyRef fakeState \state -> state { dnode = dnode }
  pure env


-- | Queue a command that will be applied to the model.
queueCommand
  :: forall eff model msg
  .  Program model msg
  -> Cmd msg
  -> Eff (ref::REF|eff) Unit
queueCommand env cmd = do
  modifyRef env.pending $ appendCmd cmd
  pure unit
  where
    appendCmd cmd pending = pending <> cmd


-- | Cmd emitter for the VirtualDom
emitter
  :: forall eff model msg
  .  Program model msg
  -> Cmd msg
  -> Eff (ref::REF,dom::DOM|eff) Unit
emitter env cmd = do
  queueCommand env cmd
  -- XXX requestAnimationFrame for bonus points
  step env

step
  :: forall eff model msg
  .  Program model msg
  -> Eff (dom::DOM,ref::REF|eff) Unit
step env = do
  msgs <- liftEff $ modifyRef' env.pending $ \ms -> {state: [], value: ms}
  state <- liftEff $ readRef env.state

  let model2 = foldl env.updater state.model msgs
  let vnode2 = env.renderer model2
  let patch = diff state.vnode vnode2

  dnode2 <- liftEff $ applyPatches (emitter env) state.dnode state.vnode patch

  writeRef env.state {model: model2, vnode: vnode2, dnode: dnode2}
  pure unit
