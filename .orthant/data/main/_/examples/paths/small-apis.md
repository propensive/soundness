# A path, written as text and read back

A path becomes text with `encode`, and text becomes a path with `as`, the methods which do
the same for a number or a date. The platform in the path's type supplies the separator and
the root.

```scala fuel
compile
frame
  label A Windows path
  code
      import strategies.throwUnsafely

      val house: Path on Windows = Drive('D') / "Verona" / "Capulet"
frame
  label As text
  highlight encode  with the platform's separator
  code
      import strategies.throwUnsafely

      val house: Path on Windows = Drive('D') / "Verona" / "Capulet"
      val text: Text = house.encode
frame
  label And a path again
  highlight as[Path..Windows]  the platform whose rules apply
  code
      import strategies.throwUnsafely

      val house: Path on Windows = Drive('D') / "Verona" / "Capulet"
      val text: Text = house.encode
      val again = text.as[Path on Windows]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
Out.println(again.name)
```

[Text and paths](../../soundness/modules/paths.md#text-and-paths) has both directions, and
[small APIs](../../philosophy/small-apis.md) the reasons for one name.
