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
newtype Executor context effect =
  Executor (forall result. effect result -> context result)

{-|
Use the executor to run an effect in a context.
-}
execute :: Executor context effect -> effect result -> context result
execute (Executor def) effect =
  def effect

{-|
Map the effect part of an executor (contravariantly).
-}
mapEffect :: (forall result. newEffect result -> oldEffect result) -> Executor context oldEffect -> Executor context newEffect
mapEffect mapping (Executor fn) =
  Executor (fn . mapping)

{-|
Map the context part of an executor.
-}
mapContext :: (forall result. oldContext result -> newContext result) -> Executor oldContext effect -> Executor newContext effect
mapContext mapping (Executor fn) =
  Executor (mapping . fn)
