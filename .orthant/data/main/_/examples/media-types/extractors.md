# Content recognized by a pattern

Bytes often arrive without a label which can be trusted. A `Media["…"]` pattern looks in the
content for the signature of a format, and binds the evidence that it was found. The media
type in the pattern is checked against the registry like any other.

```scala fuel
compile
frame
  label One format
  code
      import detectables.{gifDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png"](png) => t"a PNG"
        case _                       => t"something else"
frame
  label Another
  highlight Media["image/gif"](gif)  tried when the first does not match
  code
      import detectables.{gifDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png"](png) => t"a PNG"
        case Media["image/gif"](gif) => t"a GIF"
        case _                       => t"something else"
frame
  label A media type which does not exist
  errors show
  code
      import detectables.{gifDetectable, pngDetectable}

      def identify(content: Data): Text = content match
        case Media["image/png"](png) => t"a PNG"
        case Media["image/gip"](gif) => t"a GIF"
        case _                       => t"something else"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(identify(hex"89504e470d0a1a0a0000000d49484452"))
Out.println(identify(hex"474946383961010001000000"))
Out.println(identify(hex"cafebabe"))
```

[Detecting a media type](../../soundness/modules/media-types.md#detecting-a-media-type)
describes what the bound value is evidence of, and how a stream is matched.
