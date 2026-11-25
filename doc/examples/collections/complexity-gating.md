# The cost of counting a list

A `List` is a linked list, so finding its size means walking all of it. That is easy to
forget inside a loop, so `size` does not compile until the cost has been acknowledged.

```scala fuel
compile
frame
  label Counting a linked list
  errors show
  code
      val cast = List(t"Lear", t"Kent", t"Fool")
      val count = cast.size
frame
  label The cost acknowledged
  highlight import..linearSize  the walk is now on the record
  code
      import dysasymptotics.linearSize

      val cast = List(t"Lear", t"Kent", t"Fool")
      val count = cast.size
##
```

[Positions and access](../../soundness/modules/collections.md#positions-and-access) lists the
other acknowledgments.
