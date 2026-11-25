# A lens onto the annotated field

A library which needs "the field marked as the identifier" does not need its name. The
instance which finds the annotation offers a lens onto the field, and the lens reads it and
replaces it.

```scala fuel
compile
frame
  label The instance for the marked field
  code
      final case class ident() extends StaticAnnotation

      case class Person(name: Text, @ident email: Text)

      val romeo = Person(t"Romeo", t"romeo@example.com")
      val identity = summon[Person is Annotated by ident]
frame
  label Its lens, reading and updating
  highlight identity.lens  a lens onto email
  code
      final case class ident() extends StaticAnnotation

      case class Person(name: Text, @ident email: Text)

      val romeo = Person(t"Romeo", t"romeo@example.com")
      val identity = summon[Person is Annotated by ident]

      val address: Text = identity.lens(romeo)
      val moved: Person = identity.lens.update(romeo, t"romeo@mantua.example")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(address)
Out.println(moved.inspect)
```

[Lenses as values](../../soundness/modules/optics.md#lenses-as-values) describes the lens, and
[composability](../../philosophy/composability.md) why the two libraries need no adapter.
