module Test.JSDOM
  ( fireClick
  , jsdomDocument
  , jsdomWindow)
where

import Prelude

import Bonsai.DOM (Document, Element, Window, document)
import Data.Foreign (F)
import Data.Function.Uncurried (Fn1, Fn2, runFn2)


foreign import primitives ::
  { jsdomWindow :: Fn1 String Window
  , simulantFire :: Fn2 String Element Unit
  }

-- | Create a JSDOM Window
jsdomWindow :: String -> Window
jsdomWindow =
  primitives.jsdomWindow

-- | Create a JSDOM Document (not returning the window)
jsdomDocument :: String -> F Document
jsdomDocument html =
  jsdomWindow html # document


-- | Fire an event using simulant.
-- |
-- | Returns the element for easy chaining.
simulantFire :: String -> Element -> F Element
simulantFire ev elem = do
  let _ = runFn2 primitives.simulantFire ev elem
  pure elem


fireClick :: Element -> F Element
fireClick =
  simulantFire "click"
