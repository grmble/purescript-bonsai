-- | Bonsai main module for imports
-- |
-- | View code will also want Bonsai.Html and Bonsai.Event
module Bonsai
  ( module Bonsai.DOM
  , module Bonsai.Types
  , module Bonsai.VirtualDom
  , module Bonsai.Core
  )
where

import Bonsai.Core (Program, UpdateResult, ProgramState, debugProgram, program, plainResult, mapResult)
import Bonsai.DOM (domElementById)
import Bonsai.Types (Cmd(..), EventDecoder, pureCommand, emptyCommand, nowCommand, laterCommand, delayCommand)
import Bonsai.VirtualDom (VNode, Property, Options, Patch
  , node, text, property, attribute, attributeNS, style
  , keyedNode)
