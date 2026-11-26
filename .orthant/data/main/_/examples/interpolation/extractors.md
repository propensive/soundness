# A literal which builds, and takes apart

An interpolator runs in both directions. The `j"…"` which writes a JSON document with a value
substituted into it is, in a `case`, a pattern which matches a document of that shape and
binds what it finds in the same place.

```scala fuel
compile
frame
  label A document, built
  highlight $lines  a value goes in
  code
      val lines = 4042
      val play: Json = j"""{"title": "Hamlet", "lines": $lines}"""
frame
  label A document, matched
  highlight $count  a value comes out
  code
      import strategies.throwUnsafely

      val lines = 4042
      val play: Json = j"""{"title": "Hamlet", "lines": $lines}"""

      val length: Int = play match
        case j"""{"title": "Hamlet", "lines": $count}""" => count.as[Int]
        case _                                           => 0
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(length.show)
```

[Patterns](../../soundness/modules/interpolation.md#patterns) names the typeclass behind the
reverse direction.
