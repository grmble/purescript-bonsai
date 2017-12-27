module Test.JSDOM
  ( jsdomDocument
  , jsdomWindow)
where

import Prelude

import Bonsai (BONSAI)
import Bonsai.DOM.Primitive (document)
import Bonsai.Types (Document, Window)
import Control.Monad.Eff (Eff)

foreign import jsdomWindow :: forall eff. String -> Eff (bonsai::BONSAI|eff) Window

jsdomDocument :: forall eff. String -> Eff (bonsai::BONSAI|eff) Document
jsdomDocument html =
  jsdomWindow html >>= document