module Fx.EitherEffect
(
  EitherEffect,
  liftEffect1,
  liftEffect2,
  executor,
)
where

import Fx.Prelude
import qualified Fx.Executor as A


{-|
A sum of two effects (@effect1@ and @effect2@) to be executed
in @context@ producing @result@.
-}
newtype EitherEffect effect1 effect2 context result =
  EitherEffect (ReaderT (A.Executor effect1 context, A.Executor effect2 context) context result)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadTrans (EitherEffect effect1 effect2) where
  lift =
    EitherEffect . lift

{-|
Lift the first of the two effects.
-}
liftEffect1 :: effect1 result -> EitherEffect effect1 effect2 context result
liftEffect1 effect =
  EitherEffect (ReaderT (\(executor, _) -> A.execute effect executor))

{-|
Lift the second of the two effects.
-}
liftEffect2 :: effect2 result -> EitherEffect effect1 effect2 context result
liftEffect2 effect =
  EitherEffect (ReaderT (\(_, executor) -> A.execute effect executor))

{-|
Compose the executors of each effect into an executor of either.
-}
executor
  :: A.Executor effect1 context
  -> A.Executor effect2 context
  -> A.Executor (EitherEffect effect1 effect2 context) context
executor executor1 executor2 =
  A.Executor (\(EitherEffect reader) -> runReaderT reader (executor1, executor2))
