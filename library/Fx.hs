module Fx
(
  BothEffects,
  liftEffect1,
  liftEffect2,
  executorOfBothEffects,
  Executor(..),
  executeEffect,
)
where

import Fx.Prelude


{-|
A sum of two effects (@effect1@ and @effect2@) to be executed
in @context@ resulting in @result@.
-}
newtype BothEffects effect1 effect2 context result =
  BothEffects (ReaderT (Executor effect1 context, Executor effect2 context) context result)
  deriving (Functor, Applicative, Monad, MonadIO)

{-|
Lift the first of the two effects.
-}
liftEffect1 :: effect1 result -> BothEffects effect1 effect2 context result
liftEffect1 effect =
  BothEffects (ReaderT (\(executor, _) -> executeEffect effect executor))

{-|
Lift the second of the two effects.
-}
liftEffect2 :: effect2 result -> BothEffects effect1 effect2 context result
liftEffect2 effect =
  BothEffects (ReaderT (\(_, executor) -> executeEffect effect executor))

{-|
Executor of a single effect.
-}
newtype Executor effect context =
  Executor (forall result. effect result -> context result)

{-|
Compose the executors of each effect into an executor of both.
-}
executorOfBothEffects
  :: Executor effect1 context
  -> Executor effect2 context
  -> Executor (BothEffects effect1 effect2 context) context
executorOfBothEffects executor1 executor2 =
  Executor (\(BothEffects reader) -> runReaderT reader (executor1, executor2))

{-|
Use the executor to run an effect in a context.
-}
executeEffect :: effect result -> Executor effect context -> context result
executeEffect effect (Executor def) =
  def effect
