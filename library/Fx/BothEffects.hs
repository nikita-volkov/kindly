module Fx.BothEffects
(
  BothEffects,
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
newtype BothEffects effect1 effect2 context result =
  BothEffects (ReaderT (A.Executor effect1 context, A.Executor effect2 context) context result)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadTrans (BothEffects effect1 effect2) where
  lift =
    BothEffects . lift

{-|
Lift the first of the two effects.
-}
liftEffect1 :: effect1 result -> BothEffects effect1 effect2 context result
liftEffect1 effect =
  BothEffects (ReaderT (\(executor, _) -> A.execute effect executor))

{-|
Lift the second of the two effects.
-}
liftEffect2 :: effect2 result -> BothEffects effect1 effect2 context result
liftEffect2 effect =
  BothEffects (ReaderT (\(_, executor) -> A.execute effect executor))

{-|
Compose the executors of each effect into an executor of both.
-}
executor
  :: A.Executor effect1 context
  -> A.Executor effect2 context
  -> A.Executor (BothEffects effect1 effect2 context) context
executor executor1 executor2 =
  A.Executor (\(BothEffects reader) -> runReaderT reader (executor1, executor2))
