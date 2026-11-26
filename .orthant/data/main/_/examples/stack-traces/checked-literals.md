# A class name which could name a class

An `Fqcn` is a fully-qualified class name, and `fqcn"…"` is checked against the JVM's rules as
the code compiles. A segment which is a Java keyword, or which holds a character no identifier
may contain, is a compile error.

```scala fuel
compile
frame
  label A segment which is a keyword
  errors show
  code
      val heroine: Fqcn = fqcn"illyria.new.Viola"
frame
  label A segment with a hyphen
  errors show
  code
      val heroine: Fqcn = fqcn"twelfth-night.court.Viola"
frame
  label A name the JVM would accept
  highlight illyria.court.Viola  every segment is a valid identifier
  code
      val heroine: Fqcn = fqcn"illyria.court.Viola"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(heroine.packageName)
Out.println(heroine.className)
```

[Class names](../../soundness/modules/stack-traces.md#class-names) says where such a name
travels, and [safety by construction](../../philosophy/safety-by-construction.md) why it is
checked where it is written.
