# A path on a platform

`Path on Linux` and `Path on Windows` are different types, each read as it is written. A path
which descends from a drive is not a path on Linux, and the compiler does not take it for one.

```scala fuel
compile
frame
  label A drive, on Linux
  errors show
  code
      val house: Path on Linux = Drive('C') / "Verona" / "Capulet"
frame
  label A drive, on Windows
  highlight on..Windows  the platform of a drive
  code
      val house: Path on Windows = Drive('C') / "Verona" / "Capulet"
frame
  label A root, on Linux
  highlight on..Linux  the platform of this root
  code
      val house: Path on Linux = % / "verona" / "capulet"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(house.encode)
```

[Platforms](../../soundness/modules/paths.md#platforms) describes each platform, and
[infix types](../../philosophy/infix-types.md) the words which types like this are made of.
