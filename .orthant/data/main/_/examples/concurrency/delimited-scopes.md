# A background loop which ends with its scope

A daemon is work which is started and left. The scope which started it does not wait for it,
and cancels it on the way out, so a daemon runs for exactly as long as its scope, and there is
nothing to remember to stop.

```scala fuel
compile
frame
  label A scope
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val play = supervise:
        t"Exeunt"
frame
  label A prompter, for as long as the play lasts
  highlight daemon  canceled when the scope ends
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val play = supervise:
        daemon(snooze(5.0*Second))
        t"Exeunt"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(play)
```

`Exeunt` is printed at once, and not after five seconds.
[Daemons](../../soundness/modules/concurrency.md#daemons) explains why a daemon cannot use its
parent's error handler, and [delimited scopes](../../philosophy/delimited-scopes.md) the idea
of a lifetime which is a block.
