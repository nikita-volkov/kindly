module Fx.Executor
(
  Executor(..),
  mapEffect,
  mapContext,
  execute,
)
where

import Fx.Prelude


{-|
An abstraction over the execution of @effect@ in @context@, both producing the same @result@.
-}
newtype Executor effect context =
  Executor (forall result. effect result -> context result)

{-|
Use the executor to run an effect in a context.
-}
execute :: effect result -> Executor effect context -> context result
execute effect (Executor def) =
  def effect

{-|
Map the effect part of an executor (contravariantly).
-}
mapEffect :: (forall result. newEffect result -> oldEffect result) -> Executor oldEffect context -> Executor newEffect context
mapEffect mapping (Executor fn) =
  Executor (fn . mapping)

{-|
Map the context part of an executor.
-}
mapContext :: (forall result. oldContext result -> newContext result) -> Executor effect oldContext -> Executor effect newContext
mapContext mapping (Executor fn) =
  Executor (mapping . fn)
