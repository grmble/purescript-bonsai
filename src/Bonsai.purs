-- | Bonsai main module for imports
-- |
-- | View code will also want Bonsai.Html and Bonsai.Event
module Bonsai
  ( module Bonsai.DOM
  , module Bonsai.Types
  , module Bonsai.Core
  )
where

import Bonsai.Core (Program, UpdateResult, ProgramState, debugProgram, emitMessage, emittingTask, issueCommand, mapResult, plainResult, program, simpleTask)
import Bonsai.DOM (domElementById)
import Bonsai.Types (BONSAI, Cmd(..), EventDecoder, pureCommand, emptyCommand)
