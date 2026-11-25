# A question about a field, asked as a type

The annotations of a type are read by summoning a typeclass, and the type which is summoned is
the question: which type, annotated by what, on which field. Each word narrows it.

```scala fuel
compile
frame
  label Every annotation on a field
  highlight Speech..on  which type, and which field
    line 6
  code
      final case class ident() extends StaticAnnotation
      final case class primary() extends StaticAnnotation

      case class Speech(speaker: Text, @ident @primary line: Long)

      val annotations = summon[Speech is Annotated on "line"]()
frame
  label Only those of one type
  highlight by..ident  which annotation
  code
      final case class ident() extends StaticAnnotation
      final case class primary() extends StaticAnnotation

      case class Speech(speaker: Text, @ident @primary line: Long)

      val annotations = summon[Speech is Annotated by ident on "line"]()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(annotations.inspect)
```

The answer is a constant in the compiled program, and no reflection finds it.
[Reading annotations](../../soundness/modules/annotations.md#reading-annotations) has every form
of the question, and [infix types](../../philosophy/infix-types.md) its vocabulary.
