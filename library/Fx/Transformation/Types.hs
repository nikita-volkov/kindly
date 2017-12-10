module Fx.Transformation.Types
where

import Fx.Prelude


{-|
Natural transformation.
An abstraction over the transformation of kind-2 @old@ to @new@, both producing the same @result@.
-}
type old --> new =
  forall result. old result -> new result
