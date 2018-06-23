module Bonsai.Core
  ( DebugOptions
  , Program
  , ProgramState

  , debugProgram
  , fullDebug
  , issueCommand
  , issueCommand'
  , noDebug
  , program
  )
where

import Prelude

import Bonsai.DOM (Document, Element, ElementId(ElementId), Window, appendChild, clearElement, document, effF, elementById, foreignErrorMsg, requestAnimationFrame)
import Bonsai.Debug (debugJsonObj, debugTiming, logJsonObj, startTiming)
import Bonsai.Types (Cmd(..), TaskContext)
import Bonsai.VirtualDom (VNode, render, diff, applyPatches)
import Effect.Aff (Aff, joinFiber, launchAff_, runAff_, suspendAff)
import Effect.Aff.AVar as AVar
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.Partial as AP
import Data.Either (Either(..))
import Data.Foldable (for_)
import Foreign (F)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)


-- | Program describes the Bonsai program.
-- |
-- | It is passed around in a ReaderT and stores
-- | callbacks and a ref to the pending commands.
-- | Event callbacks append to the list of pending
-- | commands, they will then be applied
-- | in the main event loop.
type Program model msg =
  { dbg      :: DebugOptions
  , updater  :: msg -> model -> Tuple (Cmd msg) model
  , renderer :: model -> VNode msg
  , pending  :: Ref.Ref (Array msg)
  , state    :: Ref.Ref (ProgramState model msg)
  , delayed  :: Ref.Ref (Array (AVar.AVar Unit))
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
  :: forall model msg
  .  ElementId
  -> (msg -> model -> Tuple (Cmd msg) model)
  -> (model -> VNode msg)
  -> model
  -> F Window
  -> Effect (Program model msg)
program =
  debugProgram noDebug


debugProgram
  :: forall model msg
  . DebugOptions
  -> ElementId
  -> (msg -> model -> Tuple (Cmd msg) model)
  -> (model -> VNode msg)
  -> model
  -> F Window
  -> Effect (Program model msg)
debugProgram dbg containerId@(ElementId idStr) updater renderer model win = do
  _window <- effF $ win
  _document <- effF $ document _window
  container <- effF $ elementById containerId _document

  -- use a fake ProgramState so we have a ProgramEnv to render with
  -- (needed for the emitters)
  let vnode = renderer model
  fakeState <- Ref.new { model: model, vnode: vnode, dnode: container, dirty: false }
  pending   <- Ref.new []
  delayed   <- Ref.new []
  let env = { dbg, updater, renderer, pending, state: fakeState
            , delayed, window: _window, document: _document
            }

  ts <- startTiming
  let dnode = render env.document (emitter env) vnode
  _ <- effF (clearElement container >>= appendChild dnode)
  debugTiming env.dbg.timing "render/appendChild" ts

  _ <- Ref.modify (\state -> state { dnode = dnode }) fakeState
  pure env


-- | Queue messages that will be applied to the model.
queueMessages
  :: forall model msg
  .  Program model msg
  -> Array msg
  -> Effect Unit
queueMessages env msgs =
  if Array.null msgs
    then pure unit
    else do
      debugJsonObj env.dbg.events "queue messages:" msgs
      _ <- Ref.modify (\pending -> pending <> msgs) env.pending
      pure unit

-- | Error callback for the Aff commands
emitError :: Error -> Effect Unit
emitError err =
  logJsonObj "cmd error: " err

-- | Success callback for the Aff commands
-- |
-- | this will also step the model and redraw - it's called
-- | asynchronously, we can't batch messages.
emitSuccess
  :: forall model msg
  .  Program model msg
  -> Array msg
  -> Effect Unit
emitSuccess env msgs = do
    queueMessages env msgs
    if Array.null msgs
      then pure unit
      else updateAndRedraw env

-- | Queue a commands messages (or start tasks).
-- |
-- | Will return true if a redraw is necessary
queueCommand
  :: forall model msg
  .  Program model msg
  -> Cmd msg
  -> Effect Boolean
queueCommand env cmd =
  case cmd of
    Cmd ms ->
      queueMs ms
    TaskCmd task -> do
      runAff_ emitEither (taskAff task)
      pure false
  where
    taskAff task = do
      ctx <- taskContext env
      fib <- suspendAff (task ctx)
      AVar.put fib ctx.fiber
      joinFiber fib
    queueMs msgs = do
      queueMessages env msgs
      pure $ not $ Array.null msgs
    emitEither e =
      case e of
        Left err -> emitError err
        Right _ -> pure unit

-- | Issue a command - do not delay.
-- |
-- | For pure commands, this will send the messages and request a redraw.
-- | For (asynchronous) task commands, the task will be started.
-- |
-- | This can be used to start tasks, e.g. to initialize the model.
-- |
-- | Note that Bonsai will render to the DOM some time in
-- | the future via requestAnimationFrame.  If you depend
-- | on the state of the DOM, you should use issueCommand'
-- | which will delay until the model has been rendered.
issueCommand
  :: forall model msg
  .  Program model msg
  -> Cmd msg
  -> Effect Unit
issueCommand env cmd = do
  mustUpdate <- queueCommand env cmd
  if mustUpdate
    then updateAndRedraw env
    else pure unit

-- | Issue a command and delay until the model has been rendered.
issueCommand'
  :: forall model msg
  .  Program model msg
  -> Cmd msg
  -> Aff Unit
issueCommand' env cmd = do
  liftEffect $ issueCommand env cmd
  delayUntilClean env


-- | Obtain a task context for a bonsai program.
-- |
-- | The task context can be used with emitMessage
taskContext
  :: forall model msg
  .  Program model msg
  -> Aff (TaskContext msg)
taskContext env = do
  avar <- AVar.empty
  pure
    { emitter: emitTheTypeIsALie
    , delay: delayUntilClean env
    , fiber: avar
    , document: env.document
    }
  where
    emitTheTypeIsALie msg = emitSuccess env [ msg ]


-- | Cmd emitter for the VirtualDom
-- |
-- | This is passed into the virtual dom js and calls our callbacks.
-- |
-- | True means: log the original event
emitter
  :: forall model msg
  .  Program model msg
  -> F (Cmd msg)
  -> Effect Boolean
emitter env fcmd =
  case runExcept fcmd of
    Left err -> do
      emitError (error $ foreignErrorMsg err) *> pure true
    Right cmd -> do
      mustUpdate <- queueCommand env cmd
      if mustUpdate
        then updateAndRedraw env *> pure env.dbg.events
        else pure env.dbg.events

-- | Update the model from queued messages, then redraw
updateAndRedraw
  :: forall model msg
  .  Program model msg
  -> Effect Unit
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
  :: forall model msg
  .  Program model msg
  -> Effect Unit
updateModel env = do
  msgs <- Ref.modify' (\ms -> {state: [], value: ms}) env.pending

  if Array.null msgs
    then pure unit
    else do

      -- apply all the messages in one block
      state <- liftEffect $ Ref.read env.state
      Tuple cmds model2 <- applyMessages (Tuple [] state.model) msgs
      Ref.write (state {model = model2, dirty = true}) env.state

      -- then queue all the new commands ...
      for_ cmds (queueCommand env)

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
  :: forall model msg
  .  Program model msg
  -> Effect Unit
redrawModel env = do
  state <- liftEffect $ Ref.read env.state
  if state.dirty
    then do
      ts1 <- startTiming
      let vnode2 = env.renderer state.model
      debugTiming env.dbg.timing "render" ts1

      ts2 <- startTiming
      let patch = diff state.vnode vnode2
      dnode2 <- liftEffect $ applyPatches env.document (emitter env) state.dnode state.vnode patch
      debugTiming env.dbg.timing "diff/applyPatches" ts2

      debugJsonObj env.dbg.patch "patch:" patch
      Ref.write (state {vnode = vnode2, dnode = dnode2, dirty = false}) env.state

      delayed <- Ref.modify' (\d -> {state: [], value: d}) env.delayed
      launchAff_ $ continueDelayed delayed
      pure unit
    else
      pure unit

-- | Continue the delayed computations
continueDelayed :: Array (AVar.AVar Unit) -> Aff Unit
continueDelayed delayed =
  for_ delayed \avar -> AVar.put unit avar

-- | Delay until the model is clean again after the next render.
delayUntilClean
  :: forall model msg
  .  Program model msg
  -> Aff Unit
delayUntilClean env = do
  state <- liftEffect $ Ref.read env.state
  pending <- liftEffect $ Ref.read env.pending
  when (state.dirty || (not $ Array.null pending)) do
    avar <- AVar.empty
    _ <- liftEffect $ Ref.modify (\delayed -> Array.snoc delayed avar) env.delayed
    -- this will block until the delays are cleared out after the
    -- next render
    AVar.take avar
