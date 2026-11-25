# Setting a flag gives a new set

A set of flags is a value, and never changes. Assigning to a flag is an expression, and its
result is another set with that flag switched, so the set it was made from is still there to
be compared with.

```scala fuel
compile
frame
  label No flags
  code
      enum Humor:
        case Blood, Phlegm, Choler, Melancholy

      val calm = Flags[Humor]()
frame
  label One flag on, in a new set
  highlight calm(Humor.Choler)..true  an expression, not a statement
  code
      enum Humor:
        case Blood, Phlegm, Choler, Melancholy

      val calm = Flags[Humor]()
      val angry = calm(Humor.Choler) = true
frame
  label Another, in a third
  highlight angry(Humor.Melancholy)..true  and `angry` is as it was
  code
      enum Humor:
        case Blood, Phlegm, Choler, Melancholy

      val calm = Flags[Humor]()
      val angry = calm(Humor.Choler) = true
      val hamlet = angry(Humor.Melancholy) = true
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(calm.set.show)
Out.println(angry.set.show)
Out.println(hamlet.set.show)
```

[Setting and testing flags](../../soundness/modules/flags.md#setting-and-testing-flags) shows
the same steps with a `var`, and [immutability](../../philosophy/immutability.md) why a value
which cannot change is safe to share.
