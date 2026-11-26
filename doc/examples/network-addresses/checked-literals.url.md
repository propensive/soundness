# A URL with values in it

A value substituted into a `url"…"` literal is put in by its type: text is URL-encoded, and a
number is a port. The parts around it are still checked by the compiler.

```scala fuel
compile
frame
  label Text is encoded into the path
  highlight $title  URL-encoded
  code
      val title: Text = t"Twelfth Night"
      val port: Int = 8080

      val page = url"https://globe.example.com/plays/$title"
frame
  label A number is the port
  highlight $port
  code
      val title: Text = t"Twelfth Night"
      val port: Int = 8080

      val page = url"https://globe.example.com:$port/plays/$title"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(page.show)
```

[URLs](../../soundness/modules/network-addresses.md#urls) describes the literal and its parts.
