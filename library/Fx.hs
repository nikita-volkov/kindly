module Fx
where

import Fx.Prelude


{-|
Executor of a single effect.
-}
newtype Executor1 effect context =
  Executor1 (forall result. effect result -> context result)

{-|
A product of two executors.
-}
data Executor2 effect1 effect2 context =
  Executor2
    (Executor1 effect1 context)
    (Executor1 effect2 context)

{-|
A product of three executors.
-}
data Executor3 effect1 effect2 effect3 context =
  Executor3
    (Executor1 effect1 context)
    (Executor1 effect2 context)
    (Executor1 effect3 context)

{-|
A sum of two effects.
-}
newtype Fx2 effect1 effect2 context result =
  Fx2 (ReaderT (Executor2 effect1 effect2 context) context result)
  deriving (Functor, Applicative, Monad, MonadIO)

{-|
A sum of three effects.
-}
newtype Fx3 effect1 effect2 effect3 context result =
  Fx3 (ReaderT (Executor3 effect1 effect2 effect3 context) context result)
  deriving (Functor, Applicative, Monad, MonadIO)

