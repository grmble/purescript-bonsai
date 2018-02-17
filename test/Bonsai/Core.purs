module Test.Bonsai.Core
where

import Prelude

import Bonsai (BONSAI, Cmd, Program, emitMessage, emittingTask, program, simpleTask, unitTask)
import Bonsai.Core (issueCommand, issueCommand')
import Bonsai.DOM (DOM, Document, ElementId(..), affF, elementById, textContent, window)
import Bonsai.JSDOM (jsdomWindow, fireClick)
import Bonsai.Types (TaskContext)
import Bonsai.VirtualDom (VNode, node, on, property, text)
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free)
import Control.Plus (empty)
import Data.Tuple (Tuple(..))
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

update :: forall eff. Msg -> Model -> Tuple (Cmd (console::CONSOLE,clienteff::CLIENTEFF|eff) Msg) Model
update msg model =
  case msg of
    Inc ->
      Tuple empty $ model + 1
    Dec ->
      Tuple empty $ model - 1
    -- test compilation/types for some ways of starting tasks
    Foo ->
      Tuple (simpleTask simpleAff) model
    Bar ->
      Tuple (emittingTask emittingAff) model
    Baz ->
      Tuple (emittingTask pureAff) model
    Boo ->
      Tuple (unitTask consoleAff) model
    TaskResult ->
      Tuple empty $ model

view :: Model -> VNode Msg
view model =
  node "div" []
    [ node "span" [ property "id" "counter" ]
        [ text $ show model ]
    , node "button"
        [ property "id" "plusButton"
        , on "click" (const $ pure $ pure Inc)
        ]
        [ text "+" ]
    , node "button"
        [ property "id" "minusButton"
        , on "click" (const $ pure $ pure Dec)
        ]
        [ text "-" ]
    ]


simpleAff :: forall eff. Document -> Aff (clienteff::CLIENTEFF|eff) Msg
simpleAff _ =
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

consoleAff :: forall eff.  Document -> Aff (console::CONSOLE|eff) Unit
consoleAff _ = do
  liftEff $ log "Hello, world"
  pure unit

-- test using issueCommand from a main program
-- this is only here to make sure it compiles
main :: Eff (bonsai::BONSAI,dom::DOM,exception::EXCEPTION) Unit
main = do
  prg <- window # program (ElementId "main") update view 0
  issueCommand prg (simpleTask simpleAff)
  issueCommand prg (emittingTask emittingAff)
  pure unit


elementTextAfterRender
  :: forall eff model msg
  .  Program eff model msg
  -> ElementId
  -> Aff (dom::DOM|eff) String
elementTextAfterRender env id = do
  affF $ (\_ -> elementById id env.document >>= textContent) unit


tests :: forall eff. Free (TestF (bonsai::BONSAI,dom::DOM,clienteff::CLIENTEFF,console::CONSOLE,exception::EXCEPTION|eff)) Unit
tests =
  suite "Bonsai.Core" do
    test "program/taskContext" $ do
      env <- liftEff $
        jsdomWindow """<html><body id="main"></body></html>""" #
        program (ElementId "main") update view 0
      initialText <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "0" initialText

      issueCommand' env $ pure Inc
      x1 <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "1" x1

      -- liftEff $ issueCommand env $ pureCommand Boo
      -- observe output

      _  <- affF $
        elementById (ElementId "plusButton") env.document >>=
        fireClick
      issueCommand' env empty
      x2 <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "2" x2

    test "cmd monoid" do
      env <- liftEff $
        jsdomWindow """<html><body id="main"></body></html>""" #
        program (ElementId "main") update view 0
      initialText <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "0" initialText

      let cmd = pure Inc <> unitTask (const (delay (Milliseconds 200.0))) <> pure Inc
      issueCommand' env cmd
      delay (Milliseconds 100.0)
      textAfterInc <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "1" textAfterInc

      delay (Milliseconds 200.0)
      textAfterRest <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "2" textAfterRest
