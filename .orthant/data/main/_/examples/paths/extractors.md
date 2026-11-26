# A path, taken apart from the front

The extractor `/:` peels the root, and then each name, from the front of a path. A pattern
therefore reads in the order of the path it matches.

```scala fuel
compile
frame
  label The root, and the rest
  code
      import strategies.throwUnsafely

      val path = t"/home/viola/letters".as[Path on Linux under %.type]

      val found = path match
        case root /: rest => rest
frame
  label A name which must match
  highlight t"home"  a literal, to be matched
  code
      import strategies.throwUnsafely

      val path = t"/home/viola/letters".as[Path on Linux under %.type]

      val found = path match
        case root /: t"home" /: rest => rest
        case _                       => path.relative
frame
  label A name which is bound
  highlight user  the next name, whatever it is
  code
      import strategies.throwUnsafely

      val path = t"/home/viola/letters".as[Path on Linux under %.type]

      val found = path match
        case root /: t"home" /: user /: rest => user
        case _                               => path.relative
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(found.inspect)
```

[Deconstructing](../../soundness/modules/paths.md#deconstructing) explains why the root is
matched too.
