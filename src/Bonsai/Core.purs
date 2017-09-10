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
import Bonsai.Types (Cmd(..))
import Bonsai.VirtualDom (VNode, render, diff, applyPatches)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import Control.Plus (empty)
import DOM (DOM)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode)
import Data.Array (null, snoc)
import Data.Array.Partial (head, tail)
import Data.Either (Either(..))
import Data.Foreign (F)
import Partial.Unsafe (unsafePartial)


-- | Program describes the Bonsai program.
-- |
-- | It is passed around in a ReaderT and stores
-- | callbacks and a ref to the pending commands.
-- | Event callbacks append to the list of pending
-- | commands, they will then be applied
-- | in the main event loop.
type Program model msg =
  { dbgTiming:: Boolean
  , dbgEvents:: Boolean
  , updater  :: model -> msg -> UpdateResult model msg
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
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) (Program model msg)
program container updater renderer model =
  debugProgram container false false updater renderer model


debugProgram
  :: forall eff model msg
  .  Element
  -> Boolean
  -> Boolean
  -> (model -> msg -> UpdateResult model msg)
  -> (model -> VNode msg)
  -> model
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) (Program model msg)
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


-- | Queue a command that will be applied to the model.
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
  -> F (Cmd msg)
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Boolean
emitter env fcmd =
  case runExcept fcmd of
    Right cmd -> do
      queueCommand env cmd
      -- XXX requestAnimationFrame for bonus points
      step env
      pure true
    Left err -> do
      let _ = logJson "EventDecoder error:" err
      pure false

step
  :: forall eff model msg
  .  Program model msg
  -> Eff (console::CONSOLE,dom::DOM,ref::REF|eff) Unit
step env = do
  msgs <- liftEff $ modifyRef' env.pending $ \ms -> {state: [], value: ms}

  if null msgs
    then pure unit
    else do

      state <- liftEff $ readRef env.state

      model2 <- updateModel state.model msgs

      ts <- startTiming
      let vnode2 = env.renderer model2
      debugTiming env.dbgTiming "render" ts

      ts2 <- startTiming
      let patch = diff state.vnode vnode2
      debugTiming env.dbgTiming "diff" ts2

      ts3 <- startTiming
      dnode2 <- liftEff $ applyPatches (emitter env) state.dnode state.vnode patch
      debugTiming env.dbgTiming "applyPatches" ts3

      writeRef env.state {model: model2, vnode: vnode2, dnode: dnode2}

      -- drain the pending queue!
      step env

  where
    updateModel model [] = pure model
    updateModel model msgs = unsafePartial $ do
      let msg = head msgs
      debugJsonObj env.dbgEvents "message event:" msg
      let {model:model2, cmd:cmd} = env.updater model msg
      queueCommand env cmd
      updateModel model2 $ tail msgs
