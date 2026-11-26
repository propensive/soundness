# A fallback for an invalid name

An invalid name need not be an error. `safely` makes the result of validation an `Optional`,
and `or` supplies a literal, which the compiler has already checked, in its place.

```scala fuel
compile
frame
  label A name, or nothing
  highlight safely  an invalid name is Unset
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def tag(text: Text): Optional[Name[Tag]] = safely(Name[Tag](text))
frame
  label A name, or a default
  highlight or(n"untagged")  a literal cannot be invalid
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def tag(text: Text): Name[Tag] = safely(Name[Tag](text)).or(n"untagged")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(tag(t"release").show)
Out.println(tag(t"v1.0").show)
```

[Validating at runtime](../../soundness/modules/names.md#validating-at-runtime) shows the error
which `safely` absorbs, and [optionality](../../philosophy/optionality.md) explains `or`.
