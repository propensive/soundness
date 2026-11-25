# A tag is not a branch

Two namespaces may have the same rules today and different rules tomorrow. Each is a type of
its own, so a name which was validated for one is not accepted where the other is required.

```scala fuel
compile
frame
  label A tag, given as a branch
  errors show
  code
      sealed trait Tag
      sealed trait Branch

      inline given tags: Tag is Nominative under MustNotEqual[""] = !!
      inline given branches: Branch is Nominative under MustNotEqual[""] = !!

      def checkout(branch: Name[Branch]): Text = t"on ${branch.show}"

      val tag: Name[Tag] = n"twelfth-night"
      val status = checkout(tag)
frame
  label A branch
  highlight Name[Branch]  validated as a branch
    line 9
  code
      sealed trait Tag
      sealed trait Branch

      inline given tags: Tag is Nominative under MustNotEqual[""] = !!
      inline given branches: Branch is Nominative under MustNotEqual[""] = !!

      def checkout(branch: Name[Branch]): Text = t"on ${branch.show}"

      val branch: Name[Branch] = n"twelfth-night"
      val status = checkout(branch)
##
```

[Combining rules](../../soundness/modules/names.md#combining-rules) says why the two were never
the same thing, and [impossible states](../../philosophy/impossible-states.md) has the wider
argument.
