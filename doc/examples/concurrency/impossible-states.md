# A pause has a unit

`snooze` takes a duration, and a bare number is not one. Read as nanoseconds, `200L` would
return almost at once, and read as milliseconds it would not, so nothing reads it until a unit
has been named.

```scala fuel
compile
frame
  label Two hundred of what?
  errors show
  code
      import threads.virtualThreads, strategies.throwUnsafely

      def interval(): Unit = supervise(snooze(200L))
frame
  label A quantity of time
  highlight 0.2*Second
  code
      import threads.virtualThreads, strategies.throwUnsafely

      def interval(): Unit = supervise(snooze(0.2*Second))
frame
  label Or a reading of a bare number, chosen by name
  highlight millisecondsAbstractable
  code
      import threads.virtualThreads, strategies.throwUnsafely
      import abstractables.millisecondsAbstractable

      def interval(): Unit = supervise(snooze(200L))
##
```

[Units](../../soundness/modules/concurrency.md#units) lists the readings of a `Long`, and
[impossible states](../../philosophy/impossible-states.md) explains why an ambiguous value is
kept out.
