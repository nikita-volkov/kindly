module Fx.EitherEffect
(
  EitherEffect,
  liftLeft,
  liftRight,
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
  EitherEffect (ReaderT (A.Executor context effect1, A.Executor context effect2) context result)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadTrans (EitherEffect effect1 effect2) where
  lift =
    EitherEffect . lift

{-|
Lift the first of the two effects.
-}
liftLeft :: effect1 result -> EitherEffect effect1 effect2 context result
liftLeft effect =
  EitherEffect (ReaderT (\(executor, _) -> A.execute executor effect))

{-|
Lift the second of the two effects.
-}
liftRight :: effect2 result -> EitherEffect effect1 effect2 context result
liftRight effect =
  EitherEffect (ReaderT (\(_, executor) -> A.execute executor effect))

{-|
Compose the executors of each effect into an executor of either.
-}
executor
  :: A.Executor context effect1
  -> A.Executor context effect2
  -> A.Executor context (EitherEffect effect1 effect2 context)
executor executor1 executor2 =
  A.Executor (\(EitherEffect reader) -> runReaderT reader (executor1, executor2))
