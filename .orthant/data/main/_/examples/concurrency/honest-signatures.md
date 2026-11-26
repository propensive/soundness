# A method which waits says so

Pausing suspends the task which is running, so `snooze` needs to know which task that is. It
asks for a `Monitor`, and a method which pauses must ask for one too: that it can wait is in
its signature.

```scala fuel
compile
frame
  label A pause, in a method which does not admit to it
  errors show
  code
      def interval(): Unit = snooze(0.2*Second)
frame
  label The method asks for a monitor
  highlight Monitor
  code
      def interval()(using Monitor): Unit = snooze(0.2*Second)
frame
  label And is called where there is one
  highlight supervise
  code
      import threads.virtualThreads, strategies.throwUnsafely

      def interval()(using Monitor): Unit = snooze(0.2*Second)

      def play(): Unit = supervise:
        interval()
##
```

[Suspension as an effect](../../soundness/modules/concurrency.md#suspension-as-an-effect)
defends blocking, and [honest signatures](../../philosophy/honest-signatures.md) the habit of
declaring it.
