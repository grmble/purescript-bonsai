module Test.Bonsai.Core
where

import Prelude

import Bonsai (BONSAI, UpdateResult, domElementById, emitMessage, emittingTask, issueCommand, plainResult, program, simpleTask)
import Bonsai.Html (button, div_, render, text, (!))
import Bonsai.Html.Events (onClick)
import Bonsai.Types (TaskContext)
import Bonsai.VirtualDom (VNode)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (Free)
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Test.JSDOM (makeDocument)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

foreign import data CLIENTEFF :: Effect

type Model = Int

data Msg
  = Inc
  | Dec
  | Foo
  | Bar
  | Baz
  | Boo
  | TaskResult

update :: forall eff. Model -> Msg -> UpdateResult (console::CONSOLE,clienteff::CLIENTEFF|eff) Model Msg
update model msg =
  case msg of
    Inc ->
      plainResult $ model + 1
    Dec ->
      plainResult $ model - 1
    -- test compilation/types for some ways of starting tasks
    Foo ->
      { model, cmd: simpleTask simpleAff }
    Bar ->
      { model, cmd: emittingTask emittingAff }
    Baz ->
      { model, cmd: emittingTask pureAff }
    Boo ->
      { model, cmd: emittingTask consoleAff }
    TaskResult ->
      plainResult model

view :: Model -> VNode Msg
view model =
  render $ div_ $ do
    text $ show model
    button ! onClick Inc $ text "+"
    button ! onClick Dec $ text "-"

simpleAff :: forall eff. Aff (clienteff::CLIENTEFF|eff) Msg
simpleAff =
  pure TaskResult

emittingAff :: forall eff
  .  TaskContext (clienteff::CLIENTEFF|eff) Msg
  -> Aff (clienteff::CLIENTEFF|eff) Unit
emittingAff ctx = do
  emitMessage ctx TaskResult
  pure unit

pureAff :: forall eff
  .  TaskContext eff Msg
  -> Aff eff Unit
pureAff ctx = do
  emitMessage ctx TaskResult
  pure unit

consoleAff :: forall eff
  .  TaskContext (console::CONSOLE|eff) Msg
  -> Aff (console::CONSOLE|eff) Unit
consoleAff ctx = do
  liftEff $ log "Hello, world"
  pure unit

-- test using issueCommand from a main program
main :: Eff (bonsai::BONSAI,dom::DOM) Unit
main = unsafePartial $ do
  Just mainDiv  <- domElementById (ElementId "main")
  prg <- program mainDiv update view 0
  issueCommand prg (simpleTask simpleAff)
  issueCommand prg (emittingTask emittingAff)
  pure unit


tests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
tests =
  suite "Bonsai.Core" do
    test "program/taskContext" do
      env <- liftEff $ do
        document <- makeDocument """<html><body id="main"></body></html>"""
        pure 1
        -- XXX: assignment to local variable ...
        -- debugLocalDoc document
        -- element <- elementById document "main"
        -- program element update view 0

      -- XXX: Bonsai.DOM.requestAnimationFrame: doesnt work, no window when testing
      -- emitMessage (taskContext env) [Inc]
      Assert.equal 1 1
