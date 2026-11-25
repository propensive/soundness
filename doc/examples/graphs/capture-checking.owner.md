# A graph which has been given up cannot be changed behind its owner's back

`Dag(topology)` consumes the topology it is given: the new `Dag` takes over its storage, with no
copy. Were the old name still usable, an edit through it would change a value which is supposed
to be immutable. Separation checking tracks the transfer, and refuses any later use of the name.

```scala fuel
compile
feature experimental.captureChecking
feature experimental.separationChecking
frame
  label An edit through a name which has been consumed
  errors show
  highlight court.add
    occurrence 2
  code
      import strategies.throwUnsafely

      def succession() =
        val court = Topology[Text]()
        court.add(t"Duncan", t"Malcolm")
        val dag = Dag(court)
        court.add(t"Malcolm", t"Macbeth")
        dag
frame
  label The edits finished before the transfer
  highlight Dag(court)
  code
      import strategies.throwUnsafely

      def succession() =
        val court = Topology[Text]()
        court.add(t"Duncan", t"Malcolm")
        court.add(t"Malcolm", t"Macbeth")
        Dag(court)
##
```

[Building a graph](../../soundness/modules/graphs.md#building-a-graph) describes how a topology
is edited and then given up.
