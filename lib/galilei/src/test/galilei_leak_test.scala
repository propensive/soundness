/* Capture-checking regression cases for `Openable.open`. Uncomment a
   `def` to verify the leak is rejected. */
package galilei

import language.experimental.captureChecking

import soundness.*

object LeakTests:

  // Mock Openable instance, parameterised over a simple String resource
  // so we don't need a real filesystem.

  object Mock extends Openable:
    type Self = String
    type Operand = Unit
    type Result = Handle
    type Transport = Handle

    def initialize(s: String, opts: List[Unit]): Handle =
      new Handle(() => Stream(), _ => ())

    def handle(t: Handle): Handle = t
    def close(t: Handle): Unit = ()

  // 1. Returning the handle directly: rejected.
  // def leakDirect: Handle^ =
  //   Mock.open("file", h => h, Nil)

  // 2. Closure-via-impure-field leak (not currently caught because `Handle.reader`
  // has type `() => Stream[Data]` which is impure but not tagged `^{this}`):
  // def leakClosure: () => Stream[Data] =
  //   Mock.open("file", h => () => h.reader(), Nil)
  //
  // To close this gap, `reader`/`writer` would need an explicit capture set
  // (e.g. `() ->{this} Stream[Data]`), but that interacts badly with the
  // generic `Openable` given for paths and prevents galilei.core compiling.
