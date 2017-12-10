module Fx.Transformation
where

import Fx.Prelude
import Fx.EitherMonad.Types


{-|
Natural transformation.
An abstraction over the transformation of kind-2 @input@ to @output@, both producing the same @result@.
-}
type input --> output =
  forall result. input result -> output result

{-|
Given a natural transformation of the left monad and a natural transformation of the right monad,
produces a natural transformation of either of them.
-}
eitherMonad :: Monad either => (left --> either) -> (right --> either) -> (EitherMonad left right --> either)
eitherMonad leftTrans rightTrans (EitherMonad context) =
  context leftTrans rightTrans

mapInput :: (newInput --> oldInput) -> (oldInput --> output) -> (newInput --> output)
mapInput mapping trans =
  trans . mapping

mapOutput :: (oldOutput --> newOutput) -> (input --> oldOutput) -> (input --> newOutput)
mapOutput mapping trans =
  mapping . trans
