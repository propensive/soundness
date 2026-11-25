# A path which the type does not have

An entry of a catalog is addressed by the path of a field, written as a selection and not as
text. The compiler resolves the selection against the type, so a path to a field which does
not exist cannot be asked for.

```scala fuel
compile
frame
  label The villain has no title
  errors show
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      def villain(labels: Catalog[Play, Text]): Text =
        labels(proxy ?=> proxy.villain.title)
frame
  label The villain has a name
  highlight proxy.villain.name  resolved to an index by the compiler
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      def villain(labels: Catalog[Play, Text]): Text =
        labels(proxy ?=> proxy.villain.name)
##
```

[Addressing an entry](../../soundness/modules/catalogs.md#addressing-an-entry) explains the
proxy, and [impossible states](../../philosophy/impossible-states.md) why a key should not be
text.
