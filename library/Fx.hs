module Fx
(
  BothEffects,
  Executor(..),
  executorOfBothEffects,
)
where

import Fx.Prelude


{-|
A sum of two effects.
-}
newtype BothEffects effect1 effect2 context result =
  BothEffects (ReaderT (Executor effect1 context, Executor effect2 context) context result)
  deriving (Functor, Applicative, Monad, MonadIO)

runBothEffects
  :: BothEffects effect1 effect2 context result
  -> Executor effect1 context
  -> Executor effect2 context
  -> context result
runBothEffects (BothEffects reader) executor1 executor2 =
  runReaderT reader (executor1, executor2)

{-|
Executor of a single effect.
-}
newtype Executor effect context =
  Executor { runExecutor :: forall result. effect result -> context result }

{-|
Composes the executors of each effect into an executor of both.
-}
executorOfBothEffects
  :: Executor effect1 context
  -> Executor effect2 context
  -> Executor (BothEffects effect1 effect2 context) context
executorOfBothEffects executor1 executor2 =
  Executor (\(BothEffects reader) -> runReaderT reader (executor1, executor2))
