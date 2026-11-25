# Flags, as a set of cases

A set of flags is one word, and knows nothing of collections. `set` gives its flags as an
ordinary `Set` of the enumeration's cases, and everything a `Set` can do then applies to them.

```scala fuel
compile
frame
  label Two sets of flags
  code
      enum Humor:
        case Blood, Phlegm, Choler, Melancholy

      val hamlet = (Flags[Humor]()(Humor.Melancholy) = true)(Humor.Choler) = true
      val laertes = (Flags[Humor]()(Humor.Choler) = true)(Humor.Blood) = true
frame
  label What they have in common
  highlight intersect  an operation of `Set`, not of `Flags`
  code
      enum Humor:
        case Blood, Phlegm, Choler, Melancholy

      val hamlet = (Flags[Humor]()(Humor.Melancholy) = true)(Humor.Choler) = true
      val laertes = (Flags[Humor]()(Humor.Choler) = true)(Humor.Blood) = true

      val shared: Set[Humor] = hamlet.set.intersect(laertes.set)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shared.show)
```

[Flags as a set](../../soundness/modules/flags.md#flags-as-a-set) describes `set`, and
[composability](../../philosophy/composability.md) why a conversion is all that is needed.
