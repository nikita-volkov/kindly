module Fx.Transform
where

import Fx.Prelude
import Fx.EitherMonad.Types


{-|
Natural transformation.
An abstraction over the transformation of kind-2 @input@ to @output@, both producing the same @result@.
-}
newtype input --> output =
  Transform (forall result. input result -> output result)

{-|
Given a natural transformation of the left monad and a natural transformation of the right monad,
produces a natural transformation of either of them.
-}
eitherMonad :: Monad either => (left --> either) -> (right --> either) -> (EitherMonad left right --> either)
eitherMonad (Transform leftTrans) (Transform rightTrans) =
  Transform (\ (EitherMonad context) -> context leftTrans rightTrans)

instance Category (-->) where
  id = Transform id
  (.) (Transform left) (Transform right) = Transform (left . right)
