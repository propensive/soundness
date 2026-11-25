# Addition, field by field

Derivation is not only for codecs. With `arithmetic.addable` imported, two values of a case
class are added by adding each pair of fields, with whatever addition means for the type of
each.

```scala fuel
compile
frame
  label Text concatenates, and numbers sum
  highlight +  derived for Pair
  code
      import arithmetic.addable

      case class Pair(label: Text, count: Int)

      val sum: Pair = Pair(t"Rosen", 10) + Pair(t"crantz", 15)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(sum.inspect)
```

[Deriving beyond codecs](../../soundness/modules/derivation.md#deriving-beyond-codecs) names the
other operations which derive in the same way.
