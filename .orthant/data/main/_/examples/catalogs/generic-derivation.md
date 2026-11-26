# A value for every field of a type

`catalog` applies one function to a value and to every field within it, however deeply
nested. The entries are laid out from the structure of the type, so nothing is written for
`Play` or for `Person`.

```scala fuel
compile
frame
  label A value, and a function of any field
  highlight [field]..field.toString.tt  applied to every field, at any depth
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))

      val labels: Catalog[Play, Text] =
        catalog(play)([field] => (field: field) => field.toString.tt)
frame
  label An entry, by its path
  highlight proxy.villain.name  a path, not a key
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))

      val labels: Catalog[Play, Text] =
        catalog(play)([field] => (field: field) => field.toString.tt)

      val villain: Text = labels(proxy ?=> proxy.villain.name)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(labels.size.show)
Out.println(villain)
```

There are seven entries: the play, its two people, and the two fields of each.
[Building a catalog](../../soundness/modules/catalogs.md#building-a-catalog) gives their order.
