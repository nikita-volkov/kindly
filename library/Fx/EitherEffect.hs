module Fx.EitherEffect
(
  EitherEffect,
  liftLeft,
  liftRight,
)
where

import Fx.Prelude
import Fx.EitherEffect.Types


instance Functor either => Functor (EitherEffect left right either) where
  fmap mapping (EitherEffect either) =
    EitherEffect (\ leftContext rightContext -> fmap mapping (either leftContext rightContext))

instance Applicative either => Applicative (EitherEffect left right either) where
  pure x =
    EitherEffect (\ _ _ -> pure x)
  (<*>) (EitherEffect left) (EitherEffect right) =
    EitherEffect (\ leftContext rightContext -> left leftContext rightContext <*> right leftContext rightContext)

instance Monad either => Monad (EitherEffect left right either) where
  return = pure
  (>>=) (EitherEffect left) rightK =
    EitherEffect $ \ leftContext rightContext -> do
      leftResult <- left leftContext rightContext
      case rightK leftResult of
        EitherEffect right -> right leftContext rightContext

instance MonadIO either => MonadIO (EitherEffect left right either) where
  liftIO io =
    EitherEffect (\ _ _ -> liftIO io)

{-|
Lift the first of the two effects.
-}
liftLeft :: left result -> EitherEffect left right either result
liftLeft effect =
  EitherEffect (\ leftContext rightContext -> leftContext effect)

{-|
Lift the second of the two effects.
-}
liftRight :: right result -> EitherEffect left right either result
liftRight effect =
  EitherEffect (\ leftContext rightContext -> rightContext effect)
