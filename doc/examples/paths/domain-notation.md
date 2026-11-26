# A path, written with slashes

A path is built from the root `%` with `/`, as it is written in a shell. A relative path
starts from `?`, climbs with `^`, and is applied to an absolute path with `+`.

```scala fuel
compile
frame
  label Down from the root
  code
      val viola = % / "home" / "viola"
frame
  label Up one, and down again
  highlight ^  a step up
  code
      val viola = % / "home" / "viola"
      val neighbor = ? / ^ / "olivia"
frame
  label One applied to the other
  highlight +  a relative path, applied
  code
      val viola = % / "home" / "viola"
      val neighbor = ? / ^ / "olivia"
      val olivia = viola + neighbor
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(viola.inspect)
Out.println(neighbor.inspect)
Out.println(olivia.inspect)
```

[Building a path](../../soundness/modules/paths.md#building-a-path) lists the symbols, and
[elegant prose](../../philosophy/elegant-prose.md) has the case for notation like this.
