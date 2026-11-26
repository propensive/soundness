# An attribute a manifest may not have

A JAR need not name a main class, so reading `MainClass` from a manifest gives an `Optional`
class name. There is no `null` to forget and no exception for a library JAR, and `or` says
what the absence means.

```scala fuel
compile
frame
  label A library, with no main class
  code
      import manifestAttributes.*
      import strategies.throwUnsafely

      val library = Manifest(CreatedBy(t"Soundness"))
      val main: Optional[Fqcn] = library(MainClass)
frame
  label What its absence means
  highlight let(_.text)  only if there is one
  highlight or(t"not..runnable")  what a library is
  code
      import manifestAttributes.*
      import strategies.throwUnsafely

      val library = Manifest(CreatedBy(t"Soundness"))
      val main: Optional[Fqcn] = library(MainClass)
      val entry: Text = main.let(_.text).or(t"not runnable")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(main.inspect)
Out.println(entry)
```

[Manifests](../../soundness/modules/versioning.md#manifests) describes the attributes, and
[optionality](../../philosophy/optionality.md) explains `let` and `or`.
