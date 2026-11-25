# A parameter which may not be there

A media type may carry a character set, and most do not. Looking a parameter up gives an
optional value, and the code which needs a character set says what it assumes when none is
given.

```scala fuel
compile
frame
  label The parameter, which may be absent
  highlight Optional[Text]  no wrapper, and no `null`
  code
      def encoding(content: MediaType): Optional[Text] = content.at(t"charset")
frame
  label Used as though it were present
  errors show
  code
      def encoding(content: MediaType): Text = content.at(t"charset")
frame
  label With the default the protocol gives
  highlight or(t"ISO-8859-1")  what absence means here
  code
      def encoding(content: MediaType): Text = content.at(t"charset").or(t"ISO-8859-1")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(encoding(media"text/html"(charset = t"UTF-8")))
Out.println(encoding(media"text/html"))
```

[Structure](../../soundness/modules/media-types.md#structure) names the parts of a media type,
and [optionality](../../philosophy/optionality.md) explains `or`.
