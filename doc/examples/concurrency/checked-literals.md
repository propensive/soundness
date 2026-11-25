# A task's name is checked

A named task appears in a stack trace as a path, such as `play/scene-one`, which mirrors the
tree of scopes. A name which itself contained a separator would break that path, so the name
is a literal which the compiler checks.

```scala fuel
compile
frame
  label A name with a separator in it
  errors show
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely
      import Async.nominative

      val name = supervise(task(n"scene/one")(monitor.name).await())
frame
  label A name
  highlight n"scene-one"
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely
      import Async.nominative

      val name = supervise(task(n"scene-one")(monitor.name).await())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(name.inspect)
```

[Naming tasks](../../soundness/modules/concurrency.md#naming-tasks) describes what the names
are for, and [names](../../soundness/modules/names.md) how the rules of a name are declared.
