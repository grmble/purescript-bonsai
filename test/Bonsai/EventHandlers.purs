module Test.Bonsai.EventHandlers
where


import Prelude

import Bonsai.EventHandlers (enterEscapeKey, keyCode, targetFormValues, targetValue, targetValues)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free)
import Data.Either (Either(..), isLeft)
import Data.List.NonEmpty as NEL
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign (F, Foreign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

tests :: Free TestF Unit
tests = suite "Bonsai.EventDecoder" do
  test "targetValueEvent" do
    assertEqual "asdf" targetValue $ unsafeToForeign { target: { value : "asdf" } }
    assertLeft "no value" targetValue $ unsafeToForeign { target: { xxx: "asdf" } }
  test "targetValuesEvent" do
    assertEqual (fromFoldable [Tuple "key" (NEL.singleton "asdf")])
      targetValues $ unsafeToForeign { target: [ {name:"key", value:"asdf"} ] }
    -- contained elements without name and value are ignored
    assertEqual (fromFoldable []) targetValues $ unsafeToForeign { target: [ {nameX:"key", value:"asdf"}] }
    assertEqual (fromFoldable []) targetValues $ unsafeToForeign { target: [{name:"key", valueX:"asdf"}] }
  test "targetValuesEvent/multiple values" do
    assertEqual
      (fromFoldable [Tuple "key" (unsafePartial $ fromJust $ NEL.fromFoldable ["v1", "v2"])])
      targetValues $
        unsafeToForeign { target: [{name:"key", value:"v1"}, {name:"key", value:"v2"}] }
  test "targetValuesEvent/ignore checked unless checkbox/radio" do
    assertEqual
      (fromFoldable [Tuple "key" (unsafePartial $ fromJust $ NEL.fromFoldable ["v1", "v2"])])
      targetValues $
      unsafeToForeign { target:
          [ {name:"key", value:"v1", checked: true}
          , {name:"key", value:"v2", checked: false}
          ] }
  test "targetValuesEvent/checkbox" do
    assertEqual
      (fromFoldable [Tuple "key" (NEL.singleton "v1")])
      targetValues $
        unsafeToForeign { target:
          [ {name:"key", value:"v1", type: "checkbox", checked: true}
          , {name:"key", value:"v2", type: "checkbox", checked: false}
          ] }
  test "targetValuesEvent/radio" do
    assertEqual
      (fromFoldable [Tuple "key" (NEL.singleton "v1")])
      targetValues $
        unsafeToForeign { target:
          [ {name:"key", value:"v1", type: "radio", checked: true}
          , {name:"key", value:"v2", type: "radio", checked: false}
          ] }
  test "targetFormValuesEvent" do
    assertEqual
      (fromFoldable [Tuple "key" (NEL.singleton "asdf")])
      targetFormValues $ unsafeToForeign { target: { form: [ { name:"key", value:"asdf"} ] } }
    assertEqual (fromFoldable []) targetFormValues $ unsafeToForeign { target: {form: [{nameX:"key", value:"asdf"}] } }
    assertEqual (fromFoldable []) targetFormValues $ unsafeToForeign { target: {form: [{name:"key", valueX:"asdf"}] } }
  test "keyCodeEvent" do
    assertEqual 13 keyCode $ unsafeToForeign { keyCode: 13 }
    assertLeft "no keycode" keyCode $ unsafeToForeign { keyCodeX: 13 }
  test "enterEscapeKeyEvent" do
    assertEqual (Just (Right "asdf")) enterEscapeKey $ unsafeToForeign { target: { value: "asdf" }, keyCode: 13 }
    assertEqual (Just (Left "asdf")) enterEscapeKey $ unsafeToForeign { target: { value: "asdf" }, keyCode: 27 }
    assertEqual (Nothing) enterEscapeKey $ unsafeToForeign { target: { value: "asdf" }, keyCode: 66 }
    assertLeft "no keycode" enterEscapeKey $ unsafeToForeign { target: { value: "asdf" }, keyCodeX: 13 }
    assertLeft "no value" enterEscapeKey $ unsafeToForeign { target: { valueX: "asdf" }, keyCode: 13 }


assertLeft :: forall msg. String -> (Foreign -> F msg) -> Foreign -> Aff Unit
assertLeft msg decFn event =
  Assert.assert msg $ isLeft $ runExcept $ decFn event

assertEqual
  :: forall msg
  . Eq msg => Show msg => msg -> (Foreign -> F msg) -> Foreign -> Aff Unit
assertEqual msg decFn event =
  Assert.equal (Right msg) $ runExcept $ decFn event
