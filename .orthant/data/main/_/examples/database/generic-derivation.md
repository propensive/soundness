# A key found by its annotation

Where an entity carries its own key, a reference to it is that key and nothing more. The field
marked `@primary` is found by the compiler, which gives the reference the type of that field.

```scala fuel
compile
frame
  label The key is a field
  highlight @primary
  code
      import strategies.throwUnsafely

      case class Lover(@primary email: Text, name: Text)

      val romeo = Lover(t"romeo@example.com", t"Romeo")
      val juliet = Lover(t"juliet@example.com", t"Juliet")
frame
  label A set of values resolves a key
  highlight Lover..Text  the type of the annotated field
  code
      import strategies.throwUnsafely

      case class Lover(@primary email: Text, name: Text)

      val romeo = Lover(t"romeo@example.com", t"Romeo")
      val juliet = Lover(t"juliet@example.com", t"Juliet")

      given lovers: (Lover is Resolvable by Text) = Resolvable(Set(romeo, juliet))
frame
  label A reference holds only the key
  highlight juliet.ref
  code
      import strategies.throwUnsafely

      case class Lover(@primary email: Text, name: Text)

      val romeo = Lover(t"romeo@example.com", t"Romeo")
      val juliet = Lover(t"juliet@example.com", t"Juliet")

      given lovers: (Lover is Resolvable by Text) = Resolvable(Set(romeo, juliet))

      val key: Text = juliet.ref.key
      val name: Text = juliet.ref().name
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(key)
Out.println(name)
```

[References by key](../../soundness/modules/database.md#references-by-key) has the rest, and
[Annotations](../../soundness/modules/annotations.md#finding-the-annotated-field) describes
`@primary`.
