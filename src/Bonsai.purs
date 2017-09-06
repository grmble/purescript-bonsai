module Bonsai
  ( module Bonsai.DOM
  , module Bonsai.Types
  , module Bonsai.VirtualDom
  , Program
  , UpdateResult
  , ProgramState
  , program
  , plainResult
  , mapResult
  )
where

import Prelude

import Bonsai.DOM (domElementById)
import Bonsai.Types (Cmd(..))
import Bonsai.VirtualDom (VNode, EventDecoder, Property, Options, Patch
  , node, text, property, attribute, attributeNS, style, on, onWithOptions
  , keyedNode, render, diff, applyPatches)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef, writeRef)
import Control.Plus (empty)
import DOM (DOM)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode)
import Data.Array (null, snoc)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)


-- | Program describes the Bonsai program.
-- |
-- | It is passed around in a ReaderT and stores
-- | callbacks and a ref to the pending commands.
-- | Event callbacks append to the list of pending
-- | commands, they will then be applied
-- | in the main event loop.
type Program model msg =
  { updater  :: model -> msg -> UpdateResult model msg
  , renderer :: model -> VNode msg
  , pending  :: Ref (Array msg)
  , state    :: Ref (ProgramState model msg)
  }

-- | An update functions returns a new model and a possibly empty command
type UpdateResult model msg =
  { model :: model
  , cmd   :: Cmd msg
  }

-- | Creates an update result with empty command.
plainResult :: forall model msg. model -> UpdateResult model msg
plainResult model =
  { model: model
  , cmd: empty
  }

-- | Helper to map update results from sub-components
mapResult
  :: forall model1 msg1 model2 msg2
  .  (model1 -> model2)
  -> (msg1 -> msg2)
  -> UpdateResult model1 msg1
  -> UpdateResult model2 msg2
mapResult modelFn msgFn result =
  let { model:model2, cmd: cmd } = result
  in  { model: modelFn model2
      , cmd: msgFn <$> cmd
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
  -> (model -> msg -> UpdateResult model msg)
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


-- | Queue a message that will be applied to the model.
queueCommand
  :: forall eff model msg
  .  Program model msg
  -> Cmd msg
  -> Eff (ref::REF|eff) Unit
queueCommand env NoCmd =
  pure unit
queueCommand env (Cmd msg) = do
  modifyRef env.pending \pending -> snoc pending msg
  pure unit


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

  if null msgs
    then pure unit
    else do

      state <- liftEff $ readRef env.state

      model2 <- updateModel state.model msgs
      let vnode2 = env.renderer model2
      let patch = diff state.vnode vnode2

      dnode2 <- liftEff $ applyPatches (emitter env) state.dnode state.vnode patch

      writeRef env.state {model: model2, vnode: vnode2, dnode: dnode2}

      -- drain the pending queue!
      step env

  where
    updateModel model [] = pure model
    updateModel model msgs = unsafePartial $ do
      let msg = head msgs
      let {model:model2, cmd:cmd} = env.updater model msg
      queueCommand env cmd
      updateModel model2 $ tail msgs
