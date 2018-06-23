module Test.Main where

import Prelude

import Effect (Effect)
import Test.Bonsai.Core as BCore
import Test.Bonsai.EventHandlers as BEventHandlers
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  BCore.tests
  BEventHandlers.tests
