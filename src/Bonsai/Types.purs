-- | Bonsai Types
module Bonsai.Types
  ( Cmd (..)
  , BrowserEvent
  )
where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Foreign (F)


-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | It is a functor so event results can be mapped.
data Cmd aff msg
  = NoCmd
  | Cmd msg
  | AsyncCmd (Aff aff msg)

instance cmdFunctor :: Functor (Cmd aff) where
  map _ NoCmd =
    NoCmd
  map f (Cmd msg) =
    Cmd $ f msg
  map f (AsyncCmd aff) =
    AsyncCmd $ map f aff

-- | A BrowserEvent is simply a decoded foreign
-- |
-- | This is inherently composable - it's a full monad.
type BrowserEvent msg = F msg
