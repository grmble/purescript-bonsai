module Test.Bonsai.Core
where

import Prelude

import Bonsai (BONSAI, ElementId(..), UpdateResult, Program, elementById, emitMessage, emittingTask, issueCommand, plainResult, program, pureCommand, simpleTask, window)
import Bonsai.DOM.Primitive (textContent)
import Bonsai.Html (button, div_, render, span, text, (!))
import Bonsai.Html.Attributes (id_)
import Bonsai.Html.Events (onClick)
import Bonsai.Types (TaskContext)
import Bonsai.VirtualDom (VNode)
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Test.JSDOM (jsdomWindow)
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
    span ! id_ "counter" $ do
      text $ show model
    button ! id_ "plusButton" ! onClick Inc $ text "+"
    button ! id_ "minusButton" ! onClick Dec $ text "-"

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
-- this is only here to make sure it compiles
main :: Eff (bonsai::BONSAI,exception::EXCEPTION) Unit
main = unsafePartial $ do
  prg <- window >>=
    program (ElementId "main") update view 0
  issueCommand prg (simpleTask simpleAff)
  issueCommand prg (emittingTask emittingAff)
  pure unit


elementTextAfterRender
  :: forall eff model msg
  .  Program eff model msg
  -> ElementId
  -> Aff (bonsai::BONSAI|eff) String
elementTextAfterRender env id = do
  delay (Milliseconds 100.0) -- XXX: delay until rendered
  liftEff $ unsafePartial $ do
    Just elem <- elementById (ElementId "counter") env.document
    textContent elem


tests :: forall eff. Free (TestF (bonsai::BONSAI,clienteff::CLIENTEFF,console::CONSOLE,exception::EXCEPTION|eff)) Unit
tests =
  suite "Bonsai.Core" do
    test "program/taskContext" do
      env <- liftEff $
        jsdomWindow """<html><body id="main"></body></html>""" >>=
        program (ElementId "main") update view 0
      initialText <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "0" initialText

      liftEff $ issueCommand env $ pureCommand Inc
      textAfterInc <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "1" textAfterInc

      {--
      -- XXX: does not work - can't get it the event to fire

      liftEff $ unsafePartial $ do
        Just button <- elementById (ElementId "plusButton") env.document
        fireClick button
      textAfterClick <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "2" textAfterInc
      --}
