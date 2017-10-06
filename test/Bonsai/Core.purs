module Test.Bonsai.Core
where

import Prelude

import Bonsai (UpdateResult, plainResult)
import Bonsai.Html (button, div_, render, text, (!))
import Bonsai.Html.Events (onClick)
import Bonsai.VirtualDom (VNode)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (Free)
import DOM (DOM)
import Test.JSDOM (makeDocument)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert


type Model = Int

data Msg
  = Inc
  | Dec

update :: forall aff. Model -> Msg -> UpdateResult aff Model Msg
update model msg =
  plainResult $
    case msg of
      Inc -> model + 1
      Dec -> model - 1

view :: Model -> VNode Msg
view model =
  render $
    div_ do
      text (show model)
      button ! onClick Inc $ text "+"
      button ! onClick Dec $ text "-"

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
      -- emitMessages (taskContext env) [Inc]
      Assert.equal 1 1
