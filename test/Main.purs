module Test.Main where

import Prelude

import Bonsai (BONSAI)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.Bonsai.Core (CLIENTEFF)
import Test.Bonsai.Core as BCore
import Test.Bonsai.DOM as BDOM
import Test.Bonsai.EventHandlers as BEventHandlers
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

main :: forall t1.
  Eff
    ( avar :: AVAR
    , bonsai :: BONSAI
    , clienteff :: CLIENTEFF
    , console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , exception :: EXCEPTION
    | t1
    )
    Unit
main = runTest do
  BCore.tests
  BEventHandlers.tests
  unsafeCoerce $ BDOM.tests
