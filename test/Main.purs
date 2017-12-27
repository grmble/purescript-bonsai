module Test.Main where

import Prelude

import Bonsai (BONSAI)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Test.Bonsai.Core as BCore
import Test.Bonsai.DOM as BDOM
import Test.Bonsai.EventDecoder as BEventDecoder
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

main :: forall t1.
  Eff
    ( console :: CONSOLE
    , bonsai :: BONSAI
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , ref :: REF
    | t1
    )
    Unit
main = runTest do
  BCore.tests
  BEventDecoder.tests
  unsafeCoerce $ BDOM.tests
