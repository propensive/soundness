# A name, matched against literals

The `n"…"` notation is a pattern as well as an expression. A case which could never match,
because its literal breaks the rules of the name it is compared with, is rejected as the code
compiles.

```scala fuel
compile
frame
  label Names as patterns
  highlight n"release"  checked against the rules of Tag
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def stable(tag: Name[Tag]): Boolean = tag match
        case n"release" => true
        case _          => false
frame
  label A pattern which no tag could match
  errors show
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def stable(tag: Name[Tag]): Boolean = tag match
        case n"v1.0" => true
        case _       => false
##
```

[Writing a name](../../soundness/modules/names.md#writing-a-name) describes the literal which
the pattern mirrors.
