module Test.Bonsai.Core
where

import Prelude

import Bonsai (Cmd, Program, emitMessage, emittingTask, program, simpleTask, unitTask)
import Bonsai.Core (issueCommand, issueCommand')
import Bonsai.DOM (Document, ElementId(..), affF, elementById, textContent, window)
import Bonsai.JSDOM (jsdomWindow, fireClick)
import Bonsai.Types (TaskContext)
import Bonsai.VirtualDom (VNode, node, on, property, text)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Control.Monad.Free (Free)
import Control.Plus (empty)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

type Model = Int

data Msg
  = Inc
  | Dec
  | Foo
  | Bar
  | Baz
  | Boo
  | TaskResult

update :: Msg -> Model -> Tuple (Cmd Msg) Model
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


simpleAff :: Document -> Aff Msg
simpleAff _ =
  pure TaskResult

emittingAff :: TaskContext Msg -> Aff Unit
emittingAff ctx = do
  emitMessage ctx TaskResult
  pure unit

pureAff :: TaskContext Msg
  -> Aff Unit
pureAff ctx = do
  emitMessage ctx TaskResult
  pure unit

consoleAff :: Document -> Aff Unit
consoleAff _ = do
  liftEffect $ log "Hello, world"
  pure unit

-- test using issueCommand from a main program
-- this is only here to make sure it compiles
main :: Effect Unit
main = do
  prg <- window # program (ElementId "main") update view 0
  issueCommand prg (simpleTask simpleAff)
  issueCommand prg (emittingTask emittingAff)
  pure unit


elementTextAfterRender
  :: forall model msg
  .  Program model msg
  -> ElementId
  -> Aff String
elementTextAfterRender env id = do
  affF $ (\_ -> elementById id env.document >>= textContent) unit


tests :: Free TestF Unit
tests =
  suite "Bonsai.Core" do
    test "program/taskContext" $ do
      env <- liftEffect $
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
      env <- liftEffect $
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
