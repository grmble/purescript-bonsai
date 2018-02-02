module Test.Bonsai.DOM
where

import Prelude

import Bonsai (BONSAI)
import Bonsai.DOM (ElementId(..), elementById)
import Bonsai.Types (Document(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free)
import Data.Foreign (isNull, isUndefined)
import Data.Maybe (isJust)
import Test.JSDOM (jsdomDocument)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

mainHtml :: String
mainHtml = """<html><body id="main"></body></html>"""

tests :: forall eff. Free (TestF (bonsai::BONSAI|eff)) Unit
tests =
  suite "Bonsai.DOM" do
    test "document" do
      Document doc <- liftEff $ jsdomDocument mainHtml
      Assert.assertFalse "document null" $ isNull doc
      Assert.assertFalse "document undefined" $ isUndefined doc
    test "elementById" do
      doc <- liftEff $ jsdomDocument mainHtml
      found <- liftEff $ elementById (ElementId "main") doc
      Assert.assert "found" $ isJust found
      notFound <- liftEff $ elementById (ElementId "doesNotExist") doc
      Assert.assertFalse "found" $ isJust notFound
    -- the other DOM helpers can't really be tested with our limited
    -- dom vocabulary.  they will be exercised by the full virtualdom/bonsai tests
