module Bonsai.Core
  ( DebugOptions
  , Program
  , ProgramState

  , debugProgram
  , delayUntilClean
  , fullDebug
  , issueCommand
  , noDebug
  , program
  )
where

import Prelude

import Bonsai.DOM (DOM, Document, Element, ElementId(ElementId), Window, appendChild, clearElement, document, effF, elementById, foreignErrorMsg, requestAnimationFrame)
import Bonsai.Debug (debugJsonObj, debugTiming, logJsonObj, startTiming)
import Bonsai.Types (BONSAI, Cmd(..), TaskContext)
import Bonsai.VirtualDom (VNode, render, diff, applyPatches)
import Control.Monad.Aff (Aff, joinFiber, launchAff_, liftEff', runAff_, suspendAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.Partial as AP
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (F)
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
  { dbg      :: DebugOptions
  , updater  :: msg -> model -> Tuple (Cmd aff msg) model
  , renderer :: model -> VNode msg
  , pending  :: Ref (Array msg)
  , state    :: Ref (ProgramState model msg)
  , delayed  :: Ref (Array (AVar Unit))
  , window   :: Window
  , document :: Document
  }

-- | Debug options
-- |
-- | Use noDebug or fullDebug and toggle your values.
-- |
-- | Otherwise new debug options will break your program.
-- |
-- | * dbgTiming: log render/diff/apply times
-- | * dbgEvents: log queued events (and the underlying event object)
-- | * dbgPatch: log the virtual dom patch - useful if dbgTimings are slow
type DebugOptions =
  { timing :: Boolean
  , events :: Boolean
  , patch :: Boolean
  }

-- | full debug options
fullDebug :: DebugOptions
fullDebug =
  { timing: true
  , events: true
  , patch: true
  }

-- | no debug options
noDebug :: DebugOptions
noDebug =
  { timing: false
  , events: false
  , patch: false
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
  -> (msg -> model -> Tuple (Cmd aff msg) model)
  -> (model -> VNode msg)
  -> model
  -> F Window
  -> Eff (dom::DOM,exception::EXCEPTION|eff) (Program aff model msg)
program =
  debugProgram noDebug


debugProgram
  :: forall eff aff model msg
  . DebugOptions
  -> ElementId
  -> (msg -> model -> Tuple (Cmd aff msg) model)
  -> (model -> VNode msg)
  -> model
  -> F Window
  -> Eff (dom::DOM,exception::EXCEPTION|eff) (Program aff model msg)
debugProgram dbg containerId@(ElementId idStr) updater renderer model win =
  unsafeCoerceEff $ do
    _window <- effF $ win
    _document <- effF $ document _window
    container <- effF $ elementById containerId _document

    -- use a fake ProgramState so we have a ProgramEnv to render with
    -- (needed for the emitters)
    let vnode = renderer model
    fakeState <- newRef { model: model, vnode: vnode, dnode: container, dirty: false }
    pending   <- newRef []
    delayed   <- newRef []
    let env = { dbg, updater, renderer, pending, state: fakeState
              , delayed, window: _window, document: _document
              }

    ts <- startTiming
    let dnode = render env.document (emitter env) vnode
    _ <- effF (clearElement container >>= appendChild dnode)
    debugTiming env.dbg.timing "render/appendChild" ts

    modifyRef fakeState \state -> state { dnode = dnode }
    pure env


-- | Queue messages that will be applied to the model.
queueMessages
  :: forall eff model msg
  .  Program eff model msg
  -> Array msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
queueMessages env msgs =
  if Array.null msgs
    then pure unit
    else do
      debugJsonObj env.dbg.events "queue messages:" msgs
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
    if Array.null msgs
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
      pure $ not $ Array.null msgs
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
    , delay: unsafeCoerceAff $ delayUntilClean env
    , fiber: avar
    , document: env.document
    }
  where
    emitTheTypeIsALie msg =
        unsafeCoerceEff $ emitSuccess env [ msg ]


-- | Cmd emitter for the VirtualDom
-- |
-- | This is passed into the virtual dom js and calls our callbacks.
-- |
-- | True means: log the original event
emitter
  :: forall eff model msg
  .  Program eff model msg
  -> F (Cmd (avar::AVAR,bonsai::BONSAI,ref::REF|eff) msg)
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Boolean
emitter env fcmd =
  case runExcept fcmd of
    Left err -> do
      emitError (error $ foreignErrorMsg err) *> pure true
    Right cmd -> do
      mustUpdate <- queueCommand env (unsafeCoerceCmd cmd)
      if mustUpdate
        then updateAndRedraw env *> pure env.dbg.events
        else pure env.dbg.events

-- | Update the model from queued messages, then redraw
updateAndRedraw
  :: forall eff model msg
  .  Program eff model msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
updateAndRedraw env = do
  updateModel env
  -- can't fail ... famous last words
  let _ = runExcept $ requestAnimationFrame (redrawModel env) env.window
  pure unit


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

  if Array.null msgs
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
      let msg = AP.head msgs
      let Tuple cmd model2 = env.updater msg model
      applyMessages (Tuple (Array.snoc cmds cmd) model2) $ AP.tail msgs

-- | Redraw the changed model
redrawModel
  :: forall eff model msg
  .  Program eff model msg
  -> Eff (avar::AVAR,bonsai::BONSAI,ref::REF|eff) Unit
redrawModel env = do
  state <- liftEff $ readRef env.state
  if state.dirty
    then do
      ts1 <- startTiming
      let vnode2 = env.renderer state.model
      debugTiming env.dbg.timing "render" ts1

      ts2 <- startTiming
      let patch = diff state.vnode vnode2
      dnode2 <- liftEff $ applyPatches env.document (emitter env) state.dnode state.vnode patch
      debugTiming env.dbg.timing "diff/applyPatches" ts2

      debugJsonObj env.dbg.patch "patch:" patch
      writeRef env.state (state {vnode = vnode2, dnode = dnode2, dirty = false})

      delayed <- modifyRef' env.delayed \d -> {state: [], value: d}
      launchAff_ $ continueDelayed delayed
      pure unit
    else
      pure unit

-- | Continue the delayed computations
continueDelayed :: forall eff. Array (AVar Unit) -> Aff (avar::AVAR|eff) Unit
continueDelayed delayed =
  for_ delayed \avar -> putVar unit avar

-- | Delay until the model is clean again after the next render.
delayUntilClean
  :: forall eff model msg
  .  Program eff model msg
  -> Aff (avar::AVAR,ref::REF|eff) Unit
delayUntilClean env = do
  state <- liftEff $ readRef env.state
  pending <- liftEff $ readRef env.pending
  when (state.dirty || (not $ Array.null pending)) do
    avar <- makeEmptyVar
    liftEff' $ modifyRef env.delayed \delayed -> Array.snoc delayed avar
    -- this will block until the delays are cleared out after the
    -- next render
    takeVar avar
