module Fx
(
  BothEffects,
  executorOfBothEffects,
  Executor(..),
  runExecutor,
)
where

import Fx.Prelude


{-|
A sum of two effects.
-}
newtype BothEffects effect1 effect2 context result =
  BothEffects (ReaderT (Executor effect1 context, Executor effect2 context) context result)
  deriving (Functor, Applicative, Monad, MonadIO)

{-|
Composes the executors of each effect into an executor of both.
-}
executorOfBothEffects
  :: Executor effect1 context
  -> Executor effect2 context
  -> Executor (BothEffects effect1 effect2 context) context
executorOfBothEffects executor1 executor2 =
  Executor (\(BothEffects reader) -> runReaderT reader (executor1, executor2))

{-|
Executor of a single effect.
-}
newtype Executor effect context =
  Executor (forall result. effect result -> context result)

{-|
Use the executor to run an effect in a context.
-}
runExecutor :: Executor effect context -> effect result -> context result
runExecutor (Executor def) =
  def
