# A frozen graph cannot be edited

Whether a `Topology` may be changed is not a second type, nor a flag which is checked as the
program runs. It is the capture set of the one type: a fresh topology is an exclusive, editable
value, and `freeze` gives it back with an empty capture set, on which the editing methods cannot
be called.

```scala fuel
compile
feature experimental.captureChecking
frame
  label An edge added after freezing
  errors show
  highlight frozen.add
  code
      import strategies.throwUnsafely

      def succession() =
        val court = Topology[Text]()
        court.add(t"Duncan", t"Malcolm")
        val frozen = Topology.freeze(court)
        frozen.add(t"Malcolm", t"Macbeth")
        frozen
frame
  label Every edge added first
  highlight court.add
    occurrence 2
  code
      import strategies.throwUnsafely

      def succession() =
        val court = Topology[Text]()
        court.add(t"Duncan", t"Malcolm")
        court.add(t"Malcolm", t"Macbeth")
        Topology.freeze(court)
##
```

[Freezing](../../soundness/modules/graphs.md#freezing) describes what the frozen form is laid out
for.
