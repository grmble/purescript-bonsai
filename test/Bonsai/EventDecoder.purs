module Test.Bonsai.EventDecoder
where


import Prelude

import Bonsai.EventDecoder (enterEscapeKeyEvent, keyCodeEvent, targetFormValuesEvent, targetValueEvent, targetValuesEvent)
import Bonsai.Types (EventDecoder)
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free)
import Data.Either (Either(..), isLeft)
import Data.Foreign (Foreign, toForeign)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

tests :: forall t1. Free (TestF t1) Unit
tests = suite "Bonsai.EventDecoder" do
  test "targetValueEvent" do
    assertEqual "asdf" targetValueEvent $ toForeign { target: { value : "asdf" } }
    assertLeft "no value" targetValueEvent $ toForeign { target: { xxx: "asdf" } }
  test "targetValuesEvent" do
    assertEqual (fromFoldable [Tuple "key" (singleton "asdf")])
      targetValuesEvent $ toForeign { target: [ {name:"key", value:"asdf"} ] }
    -- contained elements without name and value are ignored
    assertEqual (fromFoldable []) targetValuesEvent $ toForeign { target: [ {nameX:"key", value:"asdf"}] }
    assertEqual (fromFoldable []) targetValuesEvent $ toForeign { target: [{name:"key", valueX:"asdf"}] }
  test "targetFormValuesEvent" do
    assertEqual
      (fromFoldable [Tuple "key" (singleton "asdf")])
      targetFormValuesEvent $ toForeign { target: { form: [ { name:"key", value:"asdf"} ] } }
    assertEqual (fromFoldable []) targetFormValuesEvent $ toForeign { target: {form: [{nameX:"key", value:"asdf"}] } }
    assertEqual (fromFoldable []) targetFormValuesEvent $ toForeign { target: {form: [{name:"key", valueX:"asdf"}] } }
  test "keyCodeEvent" do
    assertEqual 13 keyCodeEvent $ toForeign { keyCode: 13 }
    assertLeft "no keycode" keyCodeEvent $ toForeign { keyCodeX: 13 }
  test "enterEscapeKeyEvent" do
    assertEqual (Just (Right "asdf")) enterEscapeKeyEvent $ toForeign { target: { value: "asdf" }, keyCode: 13 }
    assertEqual (Just (Left "asdf")) enterEscapeKeyEvent $ toForeign { target: { value: "asdf" }, keyCode: 27 }
    assertEqual (Nothing) enterEscapeKeyEvent $ toForeign { target: { value: "asdf" }, keyCode: 66 }
    assertLeft "no keycode" enterEscapeKeyEvent $ toForeign { target: { value: "asdf" }, keyCodeX: 13 }
    assertLeft "no value" enterEscapeKeyEvent $ toForeign { target: { valueX: "asdf" }, keyCode: 13 }


assertLeft :: forall msg eff. String -> EventDecoder msg -> Foreign -> Aff eff Unit
assertLeft msg decFn event =
  Assert.assert msg $ isLeft $ runExcept $ decFn event

assertEqual :: forall msg eff. Eq msg => Show msg => msg -> EventDecoder msg -> Foreign -> Aff eff Unit
assertEqual msg decFn event =
  Assert.equal (Right msg) $ runExcept $ decFn event
