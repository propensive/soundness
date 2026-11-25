# The usual names, for an identifier

A `Uuid` has few methods of its own. It is read from text with `as`, as a number is, and its
bytes are asked for with `in`, as those of any encodable value are. The type named in the
brackets is all that says a UUID is involved.

```scala fuel
compile
frame
  label Read from text
  highlight as[Uuid]  the same `as` which reads an `Int`
  code
      import strategies.throwUnsafely

      val ariel = t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".as[Uuid]
frame
  label Its sixteen bytes
  highlight in[Data]  the type wanted, in brackets
  code
      import strategies.throwUnsafely

      val ariel = t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".as[Uuid]
      val bytes: Data = ariel.in[Data]
frame
  label Its text
  highlight show  as for any value which can be shown
  code
      import strategies.throwUnsafely

      val ariel = t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c".as[Uuid]
      val bytes: Data = ariel.in[Data]
      val text: Text = ariel.show
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bytes.length.show)
Out.println(text)
```

[Rendering and bytes](../../soundness/modules/uuid.md#rendering-and-bytes) has the rest, and
[small APIs](../../philosophy/small-apis.md) the reason the names are shared.
