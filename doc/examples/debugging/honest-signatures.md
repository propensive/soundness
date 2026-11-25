# Code which inspects a program says that it needs one

Every operation on a debugged program goes through `debug`, and `debug` exists only where a
`Debug` is given. A method which suspends threads or reads frames therefore names `Debug` in its
signature, and one which does not cannot touch the debuggee.

```scala fuel
compile
frame
  label A method with no debuggee to suspend
  errors show
  code
      import strategies.throwUnsafely

      def freeze(): List[ThreadId] =
        debug.suspend()
        debug.threads()
frame
  label The session is a parameter
  highlight (using..Debug)  supplied by the session block which calls it
  code
      import strategies.throwUnsafely

      def freeze()(using Debug): List[ThreadId] =
        debug.suspend()
        debug.threads()
##
```

The example is compiled, and is not run, since it needs another JVM to debug.
[Threads and frames](../../soundness/modules/debugging.md#threads-and-frames) has more of what a
session can do, and [honest signatures](../../philosophy/honest-signatures.md) the reasoning.
