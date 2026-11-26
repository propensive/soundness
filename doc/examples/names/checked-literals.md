# A name which breaks its rules does not compile

A namespace states its rules as a type, and the `n"…"` interpolator checks a literal against
them as the code compiles. A tag which starts with `0` is rejected where it is written.

```scala fuel
compile
frame
  label A tag which starts with 0
  errors show
  code
      sealed trait Tag

      inline given Tag is Nominative under
          MustNotStart["0"] & MustNotContain["."] & MustNotEqual[""] = !!

      val tag: Name[Tag] = n"0hello"
frame
  label A tag which follows the rules
  highlight n"release"  checked as the code compiles
  code
      sealed trait Tag

      inline given Tag is Nominative under
          MustNotStart["0"] & MustNotContain["."] & MustNotEqual[""] = !!

      val tag: Name[Tag] = n"release"
##
```

[Writing a name](../../soundness/modules/names.md#writing-a-name) explains how the literal gets
its type, and [safety by construction](../../philosophy/safety-by-construction.md) why the
check belongs here.
