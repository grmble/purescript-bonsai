module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Test.Bonsai.Core as BCore
import Test.Bonsai.EventDecoder as BEventDecoder
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall t1.
  Eff
    ( console :: CONSOLE
    , dom :: DOM
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , ref :: REF
    | t1
    )
    Unit
main = runTest do
  BCore.tests
  BEventDecoder.tests
