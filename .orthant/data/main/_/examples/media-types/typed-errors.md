# Text which may not be a media type

A media type which arrives as text is parsed fully, and the parse can fail. `as[MediaType]`
does not compile until the code says what a failure does.

```scala fuel
compile
frame
  label The failure is not accounted for
  errors show
  code
      def label(header: Text): MediaType = header.as[MediaType]
frame
  label It is declared
  highlight raises..MediaType.Error  the caller decides
  code
      def label(header: Text): MediaType raises MediaType.Error = header.as[MediaType]
frame
  label Or a default takes its place
  highlight or(media"application/octet-stream")  what absence means here
  code
      def label(header: Text): MediaType =
        safely(header.as[MediaType]).or(media"application/octet-stream")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(label(t"text/html; charset=UTF-8").show)
Out.println(label(t"application").show)
```

[Structure](../../soundness/modules/media-types.md#structure) lists the reasons a
`MediaType.Error` gives.
