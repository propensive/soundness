# A difference is a value

`diff` does not change either sequence. It gives a `Diff`, which is a value like any other: it
can be kept, sent elsewhere, and applied later to the original to give the revision.

```scala fuel
compile
frame
  label Two casts
  code
      val before = List(t"Macbeth", t"Duncan", t"Banquo")
      val after = List(t"Macbeth", t"Malcolm", t"Banquo")
frame
  label The difference between them
  highlight diff  neither list is changed
  code
      val before = List(t"Macbeth", t"Duncan", t"Banquo")
      val after = List(t"Macbeth", t"Malcolm", t"Banquo")

      val succession = diff(before.to[Sequence], after.to[Sequence])
frame
  label Applied to the first, it gives the second
  highlight succession.patch(before)  a new sequence
  code
      val before = List(t"Macbeth", t"Duncan", t"Banquo")
      val after = List(t"Macbeth", t"Malcolm", t"Banquo")

      val succession = diff(before.to[Sequence], after.to[Sequence])
      val crowned: Chain[Text] = succession.patch(before)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

crowned.each(Out.println(_))
Out.println(before.inspect)
```

The original list is as it was. [Applying a patch](../../soundness/modules/diffing.md#applying-a-patch)
has more, and [immutability](../../philosophy/immutability.md) the reasons.
