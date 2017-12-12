module Fx.EitherEffect.Types
where

import Fx.Prelude

{-|
A sum of two effects.
Allows for horizontal composition of monads instead of nesting as with monad transformers.

To execute it use 'Fx.Transform.eitherEffect'.
-}
newtype EitherEffect leftEffect rightEffect context result =
  EitherEffect ((forall x. leftEffect x -> context x) -> (forall x. rightEffect x -> context x) -> context result)
