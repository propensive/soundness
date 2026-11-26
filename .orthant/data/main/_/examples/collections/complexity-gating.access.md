# The fifth element of a list, and of a sequence

Reading a `List` at a position walks from its head, and inside a loop that is a quadratic
algorithm. The read compiles once the cost is acknowledged, or once the structure is one
which does not have it.

```scala fuel
compile
frame
  label A walk which nothing has acknowledged
  errors show
  code
      val acts = List(t"I", t"II", t"III", t"IV", t"V")
      val last = acts(Quin)
frame
  label The walk, acknowledged
  highlight dysasymptotics.linearAccess  the walk is acknowledged
  code
      import dysasymptotics.linearAccess

      val acts = List(t"I", t"II", t"III", t"IV", t"V")
      val last = acts(Quin)
frame
  label A structure which reads at any position
  highlight Sequence  indexed, so there is no walk
  code
      val acts = Sequence(t"I", t"II", t"III", t"IV", t"V")
      val last = acts(Quin)
##
```

[Positions and access](../../soundness/modules/collections.md#positions-and-access) has the
table of acknowledgments.
