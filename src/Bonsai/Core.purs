module Bonsai.Core
  ( Program
  , UpdateResult
  , ProgramState
  , debugProgram
  , emittingTask
  , emitMessage
  , issueCommand
  , mapResult
  , plainResult
  , program
  , simpleTask
  , unitTask
  )
where

import Prelude

import Bonsai.DOM.Primitive (Element, ElementId(..), appendChild, clearElement, document, elementById, requestAnimationFrame)
import Bonsai.Debug (debugJsonObj, debugTiming, logJsonObj, startTiming)
import Bonsai.Types (BONSAI, Cmd(..), Document, Emitter, TaskContext, Window, emptyCommand)
import Bonsai.VirtualDom (VNode, render, diff, applyPatches)
import Control.Monad.Aff (Aff, joinFiber, runAff_, suspendAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Array (null, snoc)
import Data.Array.Partial (head, tail)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
  , window   :: Window
  , document :: Document
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
  , dirty :: Boolean
  , vnode :: VNode msg
  , dnode :: Element
  }

-- | Create initial environment for the Bonsai program.
program
  :: forall eff aff model msg
  .  ElementId
  -> (model -> msg -> UpdateResult aff model msg)
  -> (model -> VNode msg)
  -> model
  -> Window
  -> Eff (bonsai::BONSAI,exception::EXCEPTION|eff) (Program aff model msg)
program containerId updater renderer model =
  debugProgram containerId updater renderer model false false


debugProgram
  :: forall eff aff model msg
  .  ElementId
  -> (model -> msg -> UpdateResult aff model msg)
  -> (model -> VNode msg)
  -> model
  -> Boolean
  -> Boolean
  -> Window
  -> Eff (bonsai::BONSAI,exception::EXCEPTION|eff) (Program aff model msg)
debugProgram containerId@(ElementId idStr) updater renderer model dbgTiming dbgEvents _window =
  unsafeCoerceEff $ do
    _document <- document _window
    element   <- elementById containerId _document
    case element of
      Nothing ->
        throwException (error $ "element not found:" <> idStr)
      Just container -> do
        -- use a fake ProgramState so we have a ProgramEnv to render with
        -- (needed for the emitters)
        let vnode = renderer model
        fakeState <- newRef { model: model, vnode: vnode, dnode: container, dirty: false }
        pending   <- newRef []
        let env = { dbgTiming, dbgEvents, updater: updater
                  , renderer: renderer, pending: pending, state: fakeState
                  , window: _window, document: _document
                  }

        ts <- startTiming
        let dnode = render env.document (emitter env) vnode
        clearElement container
        _ <- appendChild dnode container
        debugTiming env.dbgTiming "render/appendChild" ts

        modifyRef fakeState \state -> state { dnode = dnode }
        pure env


-- | Queue messages that will be applied to the model.
queueMessages
  :: forall eff model msg
  .  Program eff model msg
  -> Array msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
queueMessages env msgs =
  if null msgs
    then pure unit
    else do
      debugJsonObj env.dbgEvents "queue messages:" msgs
      modifyRef env.pending \pending -> pending <> msgs
      pure unit

-- | Error callback for the Aff commands
emitError :: forall eff. Error -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
emitError err =
  logJsonObj "cmd error: " err

-- | Success callback for the Aff commands
-- |
-- | this will also step the model and redraw - it's called
-- | asynchronously, we can't batch messages.
emitSuccess
  :: forall eff model msg
  .  Program eff model msg
  -> Array msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
emitSuccess env msgs = do
    queueMessages env msgs
    if null msgs
      then pure unit
      else updateAndRedraw env

-- | Queue a commands messages (or start tasks).
-- |
-- | Will return true if a redraw is necessary
queueCommand
  :: forall eff model msg
  .  Program eff model msg
  -> Cmd eff msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Boolean
queueCommand env cmd =
  case cmd of
    Cmd ms -> do
      unsafeCoerceEff $ queueMs ms
    TaskCmd task -> do
      runAff_ emitEither (taskAff task)
      pure false
  where
    taskAff task = do
      ctx <- taskContext env
      fib <- unsafeCoerceAff $ suspendAff (unsafeCoerceAff $ task ctx)
      unsafeCoerceAff $ putVar fib ctx.fiber
      unsafeCoerceAff $ joinFiber fib
    queueMs msgs = do
      queueMessages env msgs
      pure $ not $ null msgs
    emitEither e =
      case e of
        Left err -> emitError err
        Right _ -> pure unit

-- | Issue a command.
-- |
-- | For pure commands, this will send the messages and request a redraw.
-- | For (asynchronous) task commands, the task will be started.
-- |
-- | This can be used to start tasks, e.g. to initialize the model
issueCommand
  :: forall bff eff model msg
  .  Program bff model msg
  -> Cmd bff msg
  -> Eff (bonsai::BONSAI|eff) Unit
issueCommand env cmd = unsafeCoerceEff $ do
  mustUpdate <- queueCommand env cmd
  if mustUpdate
    then updateAndRedraw env
    else pure unit

-- | Unsafe coerce the effects of a Cmd.
unsafeCoerceCmd :: forall eff1 eff2 msg. Cmd eff1 msg -> Cmd eff2 msg
unsafeCoerceCmd cmd = unsafeCoerce cmd

-- | Obtain a task context for a bonsai program.
-- |
-- | The task context can be used with emitMessage
taskContext
  :: forall eff model msg
  .  Program eff model msg
  -> Aff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) (TaskContext eff msg)
taskContext env = do
  avar <- makeEmptyVar
  pure
    { emitter: emitTheTypeIsALie
    , fiber: avar
    , document: env.document
    }
  where
    emitTheTypeIsALie msg =
        unsafeCoerceEff $ emitSuccess env [ msg ]


-- | Cmd emitter for the VirtualDom
-- |
-- | This is passed into the virtual dom js and calls our callbacks.
emitter
  :: forall eff model msg
  .  Program eff model msg
  -> Emitter (avar::AVAR,bonsai::BONSAI,ref::REF|eff) msg
emitter env ecmd =
  case ecmd of
    Left err ->
      emitError err *> pure true
    Right cmd -> do
      mustUpdate <- queueCommand env (unsafeCoerceCmd cmd)
      if mustUpdate
        then updateAndRedraw env *> pure false
        else pure false

-- | Emit helper for Tasks.
-- |
-- | In an emitting task, use this function to emit messages.
emitMessage :: forall aff msg. TaskContext aff msg -> msg -> Aff aff Unit
emitMessage ctx msg =
  unsafeCoerceAff $ liftEff $ ctx.emitter msg

-- | Update the model from queued messages, then redraw
updateAndRedraw
  :: forall eff model msg
  .  Program eff model msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
updateAndRedraw env = do
  updateModel env
  _ <- unsafeCoerceEff $ requestAnimationFrame (redrawModel env) env.window
  pure unit

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


-- | Update from queued messages
-- |
-- | This tries to batch messages up, asynchronous
-- | commands are a problem here
updateModel
  :: forall eff model msg
  .  Program eff model msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
updateModel env = do
  msgs <- liftEff $ modifyRef' env.pending $ \ms -> {state: [], value: ms}

  if null msgs
    then pure unit
    else do

      -- apply all the messages in one block
      state <- liftEff $ readRef env.state
      Tuple cmds model2 <- applyMessages (Tuple [] state.model) msgs
      writeRef env.state (state {model = model2, dirty = true})

      -- then queue all the new commands ...
      for_ cmds (queueCommand env <<< unsafeCoerceCmd)

      -- and repeat until no more messages are queued
      updateModel env

  where
    applyMessages (Tuple cmds model) [] = pure $ Tuple cmds model
    applyMessages (Tuple cmds model) msgs = unsafePartial $ do
      let msg = head msgs
      debugJsonObj env.dbgEvents "message event:" msg
      let {model:model2, cmd:cmd} = env.updater model msg
      applyMessages (Tuple (snoc cmds cmd) model2) $ tail msgs

-- | Redraw the changed model
redrawModel
  :: forall eff model msg
  .  Program eff model msg
  -> Eff (bonsai::BONSAI,ref::REF|eff) Unit
redrawModel env = do
  state <- liftEff $ readRef env.state
  if state.dirty
    then do
      ts <- startTiming
      let vnode2 = env.renderer state.model
      let patch = diff state.vnode vnode2
      dnode2 <- liftEff $ applyPatches env.document (emitter env) state.dnode state.vnode patch
      debugTiming env.dbgTiming "render/diff/applyPatches" ts
      writeRef env.state (state {vnode = vnode2, dnode = dnode2, dirty = false})
    else
      pure unit
