module Test.JSDOM
  ( fireClick
  , jsdomDocument
  , jsdomWindow)
where

import Prelude

import Bonsai (BONSAI)
import Bonsai.DOM (document)
import Bonsai.Types (Document, Element, Window)
import Control.Monad.Eff (Eff)

-- | Create a JSDOM Window
foreign import jsdomWindow :: forall eff. String -> Eff (bonsai::BONSAI|eff) Window

-- | Create a JSDOM Document (not returning the window)
jsdomDocument :: forall eff. String -> Eff (bonsai::BONSAI|eff) Document
jsdomDocument html =
  jsdomWindow html >>= document

-- | Fire an event using simulant.
foreign import simulantFire :: forall eff. String -> Element -> Eff (bonsai::BONSAI|eff) Unit

fireClick :: forall eff. Element -> Eff (bonsai::BONSAI|eff) Unit
fireClick =
  simulantFire "click"
