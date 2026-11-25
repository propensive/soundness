# Rules which are read from the type

The rules of a namespace are not code which runs over a string. They are one type, an
intersection of rules, which is read as a sentence: a `Tag` is nominative under these rules.

```scala fuel
compile
frame
  label A namespace whose names are not empty
  highlight Tag..under  the plane, and its rules
    line 3
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotEqual[""] = !!
frame
  label And contain no dot
  highlight &..MustNotContain["."]  a second rule
  code
      sealed trait Tag

      inline given Tag is Nominative under
          MustNotEqual[""] & MustNotContain["."] = !!
frame
  label And do not start with 0
  highlight &..MustNotStart["0"]  a third
    occurrence 2
  code
      sealed trait Tag

      inline given Tag is Nominative under
          MustNotEqual[""] & MustNotContain["."] & MustNotStart["0"] = !!
##
```

[Defining a plane](../../soundness/modules/names.md#defining-a-plane) has the details, and
[infix types](../../philosophy/infix-types.md) the reasons for types which read as phrases.
