module Bonsai.Debug
  (logAny)
where

import Prelude

import Debug.Trace (traceAny)

-- | Log a value using Debug.Trace traceAny
logAny :: forall a b. Applicative b => a -> b Unit
logAny x =
  traceAny x \_ -> pure unit
