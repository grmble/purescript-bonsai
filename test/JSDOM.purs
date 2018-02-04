module Test.JSDOM
  ( fireClick
  , jsdomDocument
  , jsdomWindow)
where

import Prelude

import Bonsai.DOM (Document, Element, Window(..), document, failNullOrUndefined)
import Data.Foreign (F, Foreign)
import Data.Function.Uncurried (Fn1, Fn2, runFn2)


foreign import primitives ::
  { jsdomWindow :: Fn1 String Foreign
  , simulantFire :: Fn2 String Element Unit
  }

-- | Create a JSDOM Window
jsdomWindow :: String -> F Window
jsdomWindow html =
  primitives.jsdomWindow html #
  failNullOrUndefined "jsdomWindow" >>=
  pure <<< Window

-- | Create a JSDOM Document (not returning the window)
jsdomDocument :: String -> F Document
jsdomDocument html =
  jsdomWindow html >>= document


-- | Fire an event using simulant.
simulantFire :: String -> Element -> F Unit
simulantFire ev elem =
  pure $ runFn2 primitives.simulantFire ev elem


fireClick :: Element -> F Unit
fireClick =
  simulantFire "click"
