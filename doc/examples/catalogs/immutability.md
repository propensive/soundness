# A new catalog from an old one

A catalog is built once and never changed. `map` gives another catalog over the same type,
with a new value at every entry, and a path which addressed the old one addresses the new.

```scala fuel
compile
frame
  label A catalog of labels
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))

      val labels: Catalog[Play, Text] =
        catalog(play)([field] => (field: field) => field.toString.tt)
frame
  label A catalog of their widths
  highlight map(_.length)  the labels are untouched
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))

      val labels: Catalog[Play, Text] =
        catalog(play)([field] => (field: field) => field.toString.tt)

      val widths: Catalog[Play, Int] = labels.map(_.length)
      val label: Text = labels(proxy ?=> proxy.hero.name)
      val width: Int = widths(proxy ?=> proxy.hero.name)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(label)
Out.println(width.show)
```

[Transforming a catalog](../../soundness/modules/catalogs.md#transforming-a-catalog) has more,
and [immutability](../../philosophy/immutability.md) the reasons.
