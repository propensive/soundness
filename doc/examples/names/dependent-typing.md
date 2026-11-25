# One literal, valid in several namespaces

The type of an `n"…"` literal is computed from its text: it is a name of every plane in scope
whose rules the text satisfies. One literal is a tag and a branch at once, and another, which
contains a slash, is only a branch.

```scala fuel
compile
frame
  label A literal which satisfies both sets of rules
  highlight Name[Tag..Branch]  a name of both planes
  code
      sealed trait Tag
      sealed trait Branch

      inline given tags: Tag is Nominative under MustNotContain["/"] = !!
      inline given branches: Branch is Nominative under MustNotStart["-"] = !!

      val both: Name[Tag & Branch] = n"illyria"
frame
  label A literal which satisfies only one
  errors show
  code
      sealed trait Tag
      sealed trait Branch

      inline given tags: Tag is Nominative under MustNotContain["/"] = !!
      inline given branches: Branch is Nominative under MustNotStart["-"] = !!

      val branch: Name[Branch] = n"feature/illyria"
      val tag: Name[Tag] = n"feature/illyria"
##
```

[Returning a precise type](../../soundness/modules/interpolation.md#returning-a-precise-type)
explains how an interpolator computes its result type.
