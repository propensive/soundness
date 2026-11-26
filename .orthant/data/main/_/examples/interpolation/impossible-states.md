# Nothing is substituted by `toString`

Scala's `s"…"` converts any value with `toString`, and prints `Player@1b6d3586` without
complaint. A substitution into `t"…"` is resolved through a typeclass, so a value with no way
of being shown cannot be put there.

```scala fuel
compile
frame
  label A value with no way of being shown
  errors show
  code
      class Player(val name: Text)

      val feste = Player(t"Feste")
      val entrance = t"Enter $feste"
frame
  label A way of being shown, declared
  highlight Player..Showable  how a player becomes text
    line 2
  code
      class Player(val name: Text)
      given Player is Showable = _.name

      val feste = Player(t"Feste")
      val entrance = t"Enter $feste"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(entrance)
```

[Substitutions](../../soundness/modules/interpolation.md#substitutions) describes the
typeclasses which admit a value to an interpolator.
