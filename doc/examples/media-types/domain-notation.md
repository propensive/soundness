# Parameters, written as named arguments

A media type is a value with typed parts, and is never a string to be concatenated. A parameter
such as the character set is added by applying the media type to a named argument.

```scala fuel
compile
frame
  label A media type
  code
      val page = media"text/html"
frame
  label With a parameter
  highlight (charset..t"UTF-8")  a parameter, by name
  code
      val page = media"text/html"(charset = t"UTF-8")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(page.show)
Out.println(page.basic)
Out.println(page.group.inspect)
Out.println(page.at(t"charset").inspect)
```

[Structure](../../soundness/modules/media-types.md#structure) names the four parts.
