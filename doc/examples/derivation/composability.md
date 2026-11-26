# One case class, four libraries

The libraries which encode JSON, carry values between classloaders, write debugging output and
do arithmetic share one derivation engine, and nothing else. A case class is defined once, and
each of them has an instance for it.

```scala fuel
compile
frame
  label A case class
  code
      case class Tally(play: Text, lines: Int)

      val first = Tally(t"Ham", 1500)
      val second = Tally(t"let", 2500)
frame
  label Added, field by field
  highlight +  derived by the arithmetic library
  code
      import arithmetic.addable

      case class Tally(play: Text, lines: Int)

      val first = Tally(t"Ham", 1500)
      val second = Tally(t"let", 2500)
      val total: Tally = first + second
frame
  label Encoded as JSON, and as JDK objects
  highlight in[Json]  derived by the JSON library
  code
      import arithmetic.addable

      case class Tally(play: Text, lines: Int)

      val first = Tally(t"Ham", 1500)
      val second = Tally(t"let", 2500)
      val total: Tally = first + second
      val json: Json = total.in[Json]
      val pojo: Pojo = total.pojo
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(total.inspect)
Out.println(json.show)
```

[Deriving beyond codecs](../../soundness/modules/derivation.md#deriving-beyond-codecs) shows the
arithmetic, and [composability](../../philosophy/composability.md) explains why the libraries
need not know of each other.
