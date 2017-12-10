module Fx.EitherMonad
(
  EitherMonad,
  liftLeft,
  liftRight,
)
where

import Fx.Prelude
import Fx.EitherMonad.Types


instance Functor (EitherMonad leftMonad rightMonad) where
  fmap mapping (EitherMonad context) =
    EitherMonad (\ leftContext rightContext -> liftM mapping (context leftContext rightContext))

instance Applicative (EitherMonad leftMonad rightMonad) where
  pure x =
    EitherMonad (\ _ _ -> return x)
  (<*>) (EitherMonad left) (EitherMonad right) =
    EitherMonad (\ leftContext rightContext -> left leftContext rightContext <*> right leftContext rightContext)

instance Monad (EitherMonad leftMonad rightMonad) where
  return = pure
  (>>=) (EitherMonad left) rightK =
    EitherMonad $ \ leftContext rightContext -> do
      leftResult <- left leftContext rightContext
      case rightK leftResult of
        EitherMonad right -> right leftContext rightContext

{-|
Lift the first of the two effects.
-}
liftLeft :: leftMonad result -> EitherMonad leftMonad rightMonad result
liftLeft effect =
  EitherMonad (\ leftContext rightContext -> leftContext effect)

{-|
Lift the second of the two effects.
-}
liftRight :: rightMonad result -> EitherMonad leftMonad rightMonad result
liftRight effect =
  EitherMonad (\ leftContext rightContext -> rightContext effect)
