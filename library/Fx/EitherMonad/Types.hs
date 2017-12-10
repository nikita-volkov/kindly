module Fx.EitherMonad.Types
where

import Fx.Prelude

{-|
A sum of two monads.
Allows for horizontal composition of monads instead of nesting as with monad transformers.

To execute it use 'Fx.Transform.eitherMonad'.
-}
newtype EitherMonad leftMonad rightMonad result =
  EitherMonad (forall context. Monad context => (forall x. leftMonad x -> context x) -> (forall x. rightMonad x -> context x) -> context result)
