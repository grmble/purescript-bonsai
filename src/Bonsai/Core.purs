module Bonsai.Core
  ( Program
  , UpdateResult
  , ProgramState
  , program
  , debugProgram
  , plainResult
  , mapResult
  )
where

import Prelude

import Bonsai.Debug (debugJsonObj, debugTiming, logJson, startTiming)
import Bonsai.Types (Cmd(..), Emitter, emptyCommand)
import Bonsai.VirtualDom (VNode, render, diff, applyPatches)
import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef, writeRef)
import DOM (DOM)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode)
import Data.Array (null, snoc)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


-- | Program describes the Bonsai program.
-- |
-- | It is passed around in a ReaderT and stores
-- | callbacks and a ref to the pending commands.
-- | Event callbacks append to the list of pending
-- | commands, they will then be applied
-- | in the main event loop.
type Program aff model msg =
  { dbgTiming:: Boolean
  , dbgEvents:: Boolean
  , updater  :: model -> msg -> UpdateResult aff model msg
  , renderer :: model -> VNode msg
  , pending  :: Ref (Array msg)
  , state    :: Ref (ProgramState model msg)
  }

-- | An update functions returns a new model and a possibly empty command
type UpdateResult aff model msg =
  { model :: model
  , cmd   :: Cmd aff msg
  }

-- | Creates an update result with empty command.
plainResult :: forall aff model msg. model -> UpdateResult aff model msg
plainResult model =
  { model: model
  , cmd: emptyCommand
  }

-- | Helper to map update results from sub-components
mapResult
  :: forall aff model1 msg1 model2 msg2
  .  (model1 -> model2)
  -> (msg1 -> msg2)
  -> UpdateResult aff model1 msg1
  -> UpdateResult aff model2 msg2
mapResult modelFn msgFn result =
  let { model:model2, cmd: cmd } = result
  in  { model: modelFn model2
      , cmd: map msgFn cmd
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
  :: forall eff aff model msg
  .  Element
  -> (model -> msg -> UpdateResult aff model msg)
  -> (model -> VNode msg)
  -> model
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) (Program aff model msg)
program container updater renderer model =
  debugProgram container false false updater renderer model


debugProgram
  :: forall eff aff model msg
  .  Element
  -> Boolean
  -> Boolean
  -> (model -> msg -> UpdateResult aff model msg)
  -> (model -> VNode msg)
  -> model
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) (Program aff model msg)
debugProgram container dbgTiming dbgEvents updater renderer model = do
  -- use a fake ProgramState so we have a ProgramEnv to render with
  -- (needed for the emitters)
  let vnode = renderer model
  fakeState <- newRef { model: model, vnode: vnode, dnode: container }
  pending   <- newRef []
  let env = { dbgTiming, dbgEvents, updater: updater
            , renderer: renderer, pending: pending, state: fakeState
            }

  ts <- startTiming
  let dnode = render (emitter env) vnode
  debugTiming env.dbgTiming "initial render" ts

  ts2 <- startTiming
  _ <- appendChild (elementToNode dnode) (elementToNode container)
  debugTiming env.dbgTiming "append child" ts2

  modifyRef fakeState \state -> state { dnode = dnode }
  pure env


-- | Queue a message that will be applied to the model.
queueMessage
  :: forall eff model msg
  .  Program eff model msg
  -> msg
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Unit
queueMessage env msg = do
  modifyRef env.pending \pending -> snoc pending msg
  pure unit

-- | Error callback for the Aff commands
emitError :: forall eff. Error -> Eff (console::CONSOLE|eff) Unit
emitError err =
  logJson "cmd error: " err

-- | Success callback for the Aff commands
-- |
-- | this will also step the model and redraw - it's called
-- | asynchronously, we can't batch messages.
emitSuccess
  :: forall eff model msg
  .  Program eff model msg
  -> msg
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Unit
emitSuccess env msg = do
    queueMessage env msg
    updateAndRedraw env

-- | Queue a command, inform if any messages need processing
queueCommand
  :: forall eff model msg
  .  Program eff model msg
  -> Cmd eff msg
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Boolean
queueCommand env cmd =
  case cmd of
    NoCmd ->
      pure false
    ErrCmd err -> do
      emitError err
      pure false
    Cmd msg -> do
      queueMessage env msg
      pure true
    Later aff -> do
      _ <- runAff emitError (emitSuccess env) (unsafeCoerceAff aff)
      pure false


-- | Cmd emitter for the VirtualDom
-- |
-- | This is passed into the virtual dom js and calls our callbacks.
emitter
  :: forall eff model msg
  .  Program eff model msg
  -> Emitter (console::CONSOLE,dom::DOM,ref::REF|eff) msg
emitter env cmd = do
  mustUpdate <- queueCommand env (unsafeCoerce cmd)
  if mustUpdate
    then updateAndRedraw env
    else pure unit


-- | Update from queued messages, then redraw
-- |
-- | This tries to batch messages up, only really possible
-- | with synchronous messages though
updateAndRedraw
  :: forall eff model msg
  .  Program eff model msg
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Unit
updateAndRedraw env = do
  msgs <- liftEff $ modifyRef' env.pending $ \ms -> {state: [], value: ms}

  if null msgs
    then pure unit
    else do

      state <- liftEff $ readRef env.state

      model2 <- updateModel state.model msgs

      ts <- startTiming
      let vnode2 = env.renderer model2
      let patch = diff state.vnode vnode2
      dnode2 <- liftEff $ applyPatches (emitter env) state.dnode state.vnode patch
      debugTiming env.dbgTiming "render/diff/applyPatches" ts

      writeRef env.state {model: model2, vnode: vnode2, dnode: dnode2}

      -- drain the pending queue!
      updateAndRedraw env

  where

    -- processQueued maybeModel = do
    --   msgs <- liftEff $ modifyRef' env.pending $ \ms -> {state: [], value: ms}
    --   if null msgs
    --     then pure maybeModel
    --     else do
    --

    updateModel model [] = pure model
    updateModel model msgs = unsafePartial $ do
      let msg = head msgs
      debugJsonObj env.dbgEvents "message event:" msg
      let {model:model2, cmd:cmd} = env.updater model msg
      _ <- queueCommand env (unsafeCoerce cmd)
      updateModel model2 $ tail msgs
