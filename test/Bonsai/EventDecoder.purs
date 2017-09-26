module Test.Bonsai.EventDecoder
where


import Prelude

import Bonsai.EventDecoder (targetFormValuesEvent, targetValueEvent, targetValuesEvent)
import Bonsai.Types (EventDecoder)
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free)
import Data.Either (Either(..), isLeft)
import Data.Foreign (Foreign, toForeign)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

tests :: forall t1. Free (TestF t1) Unit
tests = suite "Bonsai.EventDecoder" do
  test "targetValueEvent" do
    assertEqual "asdf" targetValueEvent $ toForeign { target: { value : "asdf" } }
    assertLeft "no value" targetValueEvent $ toForeign { target: { xxx: "asdf" } }
  test "targetValuesEvent" do
    assertEqual (fromFoldable [Tuple "key" "asdf"]) targetValuesEvent $ toForeign { target: [ {name:"key", value:"asdf"} ] }
    -- contained elements without name and value are ignored
    assertEqual (fromFoldable []) targetValuesEvent $ toForeign { target: [ {nameX:"key", value:"asdf"}] }
    assertEqual (fromFoldable []) targetValuesEvent $ toForeign { target: [{name:"key", valueX:"asdf"}] }
  test "targetFormValuesEvent" do
    assertEqual (fromFoldable [Tuple "key" "asdf"]) targetFormValuesEvent $ toForeign { target: { form: [ { name:"key", value:"asdf"} ] } }
    assertEqual (fromFoldable []) targetFormValuesEvent $ toForeign { target: {form: [{nameX:"key", value:"asdf"}] } }
    assertEqual (fromFoldable []) targetFormValuesEvent $ toForeign { target: {form: [{name:"key", valueX:"asdf"}] } }


assertLeft :: forall msg eff. String -> EventDecoder msg -> Foreign -> Aff eff Unit
assertLeft msg decFn event =
  Assert.assert msg $ isLeft $ runExcept $ decFn event

assertEqual :: forall msg eff. Eq msg => Show msg => msg -> EventDecoder msg -> Foreign -> Aff eff Unit
assertEqual msg decFn event =
  Assert.equal (Right msg) $ runExcept $ decFn event
