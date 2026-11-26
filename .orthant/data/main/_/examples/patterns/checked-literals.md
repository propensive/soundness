# A regular expression the compiler has read

A pattern written with `r"…"` is parsed as the code compiles. One which is not a regular
expression is a compile error, which names the fault and its position.

```scala fuel
compile
frame
  label An unclosed group
  errors show
  code
      def isGreeting(text: Text): Boolean = text match
        case r"hello( world" => true
        case _               => false
frame
  label The group closed
  highlight (..)?  a group, closed
  code
      def isGreeting(text: Text): Boolean = text match
        case r"hello( world)?" => true
        case _                 => false
##
```

[Patterns](../../soundness/modules/patterns.md#compiletime-checking) describes what else is
checked.
