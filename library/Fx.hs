module Fx
(
  Fx2,
  runFx2,
  Executor(..),
)
where

import Fx.Prelude


{-|
A sum of two effects.
-}
newtype Fx2 effect1 effect2 context result =
  Fx2 (ReaderT (Executor effect1 context, Executor effect2 context) context result)
  deriving (Functor, Applicative, Monad, MonadIO)

runFx2
  :: Fx2 effect1 effect2 context result
  -> Executor effect1 context
  -> Executor effect2 context
  -> context result
runFx2 (Fx2 reader) executor1 executor2 =
  runReaderT reader (executor1, executor2)

{-|
Executor of a single effect.
-}
newtype Executor effect context =
  Executor (forall result. effect result -> context result)
