# A pattern can detect only what was imported

What a `Media["…"]` pattern is able to recognize is decided by the detectors in scope, each
imported by name. A pattern for a format with no detector is a compile error, and not a case
which never matches.

```scala fuel
compile
frame
  label No detector for JPEG
  errors show
  code
      import detectables.pngDetectable

      def identify(content: Data): Text = content match
        case Media["image/png"](png)   => t"a PNG"
        case Media["image/jpeg"](jpeg) => t"a JPEG"
        case _                         => t"something else"
frame
  label The detector, imported
  highlight jpegDetectable  now JPEG can be detected
  code
      import detectables.{jpegDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png"](png)   => t"a PNG"
        case Media["image/jpeg"](jpeg) => t"a JPEG"
        case _                         => t"something else"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(identify(hex"89504e470d0a1a0a0000000d49484452"))
Out.println(identify(hex"ffd8ffe000104a4649460001"))
```

[Detecting a media type](../../soundness/modules/media-types.md#detecting-a-media-type) has
the rest.
