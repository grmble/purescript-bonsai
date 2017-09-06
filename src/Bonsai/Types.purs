-- | Bonsai Types
module Bonsai.Types
  ( Cmd (..)
  )
where

import Prelude

import Control.Plus (class Alt, class Plus)

-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | It is a functor so event results can be mapped.
data Cmd msg
  = NoCmd
  | Cmd msg

instance cmdFunctor :: Functor Cmd where
  map _ NoCmd =
    NoCmd
  map f (Cmd msg) =
    Cmd $ f msg

instance cmdApply :: Apply Cmd where
  apply NoCmd _ = NoCmd
  apply _ NoCmd = NoCmd
  apply (Cmd f) (Cmd msg) = Cmd $ f msg

instance cmdApplicative :: Applicative Cmd where
  pure msg = Cmd msg

instance cmdBind :: Bind Cmd where
  bind NoCmd _ = NoCmd
  bind (Cmd msg) f = f msg

instance cmdAlt :: Alt Cmd where
  alt NoCmd x = x
  alt x _ = x

instance cmdPlus :: Plus Cmd where
  empty = NoCmd
