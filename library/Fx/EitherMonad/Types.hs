module Fx.EitherMonad.Types
where

import Fx.Prelude
import Fx.Transformation.Types

{-|
A sum of two monads.
Allows for horizontal composition of monads instead of nesting as with monad transformers.

To execute it use 'Fx.Transformation.eitherMonad'.
-}
newtype EitherMonad leftMonad rightMonad result =
  EitherMonad (forall context. Monad context => (leftMonad --> context) -> (rightMonad --> context) -> context result)
