# A name which a platform does not allow

Binding a path to a platform checks each of its names against that platform's rules. A name
which holds a slash is not a name on Linux, so the path which contains it is not a path.

```scala fuel
compile
frame
  label Two names, written as one
  errors show
  code
      val letters = (% / "home" / "viola/letters").on[Linux]
frame
  label Two names
  highlight "letters"  a name of its own
  code
      val letters = (% / "home" / "viola" / "letters").on[Linux]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(letters.encode)
```

[Platforms](../../soundness/modules/paths.md#platforms) has the platforms, and
[Names](../../soundness/modules/names.md) the way their rules are stated.
