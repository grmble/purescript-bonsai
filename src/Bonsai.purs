-- | Bonsai main module for imports
-- |
-- | View code will also want Bonsai.Html and Bonsai.Event
module Bonsai
  ( module Bonsai.DOM
  , module Bonsai.Types
  , module Bonsai.VirtualDom
  , module Bonsai.Core
  , module Bonsai.Html.Internal
  )
where

import Bonsai.Core (Program, UpdateResult, ProgramState, debugProgram, emitMessages, mapResult, plainResult, program)
import Bonsai.DOM (domElementById)
import Bonsai.Html.Internal ((!), (!?), (#!), (#!?), render, text)
import Bonsai.Types (Cmd(..), EventDecoder, pureCommand, emptyCommand, simpleTask, readerTask)
import Bonsai.VirtualDom (VNode, Property)
