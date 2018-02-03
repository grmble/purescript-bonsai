module Test.Bonsai.DOM
where

import Prelude

import Bonsai (BONSAI)
import Bonsai.DOM (Document(..), ElementId(..), affF, elementById, innerHTML, ownerDocument, querySelector, querySelectorAll, textContent)
import Control.Monad.Aff (try)
import Control.Monad.Free (Free)
import Data.Array as Array
import Data.Either (isLeft)
import Data.Foreign (isNull, isUndefined)
import Data.Newtype (unwrap)
import Test.JSDOM (jsdomDocument)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

mainHtml :: String
mainHtml = """<html><body id="main">Hello, world!</body></html>"""

tests :: forall eff. Free (TestF (bonsai::BONSAI|eff)) Unit
tests =

  suite "Bonsai.DOM" do
    test "document" do
      Document doc <- affF $ jsdomDocument mainHtml
      Assert.assertFalse "document null" $ isNull doc
      Assert.assertFalse "document undefined" $ isUndefined doc

    test "elementById/textContent" do
      doc <- affF $ jsdomDocument mainHtml
      found <- affF $ elementById (ElementId "main") doc
      text  <- affF $ textContent found
      Assert.equal "Hello, world!" text

    test "elementById #doesNotExist" do
      doc <- affF $ jsdomDocument mainHtml
      err <- try $ affF $ elementById (ElementId "doesNotExist") doc
      Assert.assert "should have errored" $ isLeft err

    test "querySelector/innerHTML/ownerDocument" do
      doc <- affF $ jsdomDocument mainHtml
      f1 <- affF $ elementById (ElementId "main") doc
      i1 <- affF $ innerHTML f1
      odoc <- affF $ ownerDocument f1
      f2 <- affF $ querySelector "#main" (unwrap odoc)
      i2 <- affF $ innerHTML f2
      Assert.equal i1 i2

    test "querySelectorAll" do
      doc <- affF $ jsdomDocument mainHtml
      arr <- affF $ querySelectorAll "div" (unwrap doc)
      Assert.equal 0 (Array.length arr)
      arr2 <- affF $ querySelectorAll "body" (unwrap doc)
      Assert.equal 1 (Array.length arr2)

    -- the other DOM helpers can't really be tested with our limited
    -- dom vocabulary.  they will be exercised by the full virtualdom/bonsai tests
