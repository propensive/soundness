# Two catalogs, joined without matching names

`braid` combines two catalogs entry by entry. Both must be catalogs of the same type, so every
entry of one has its partner in the other, and the join has no case in which a key is missing.

```scala fuel
compile
frame
  label Catalogs of different types
  errors show
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      def cells(labels: Catalog[Play, Text], widths: Catalog[Person, Int]): Catalog[Play, Text] =
        labels.braid(widths)((label, width) => t"$label ($width)")
frame
  label Catalogs of the same type
  highlight Catalog[Play,..Int]  the same key type
    occurrence 2
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      def cells(labels: Catalog[Play, Text], widths: Catalog[Play, Int]): Catalog[Play, Text] =
        labels.braid(widths)((label, width) => t"$label ($width)")
##
```

[Combining catalogs](../../soundness/modules/catalogs.md#combining-catalogs) shows a table's
settings assembled this way.
