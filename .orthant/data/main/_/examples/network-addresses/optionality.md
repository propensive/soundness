# A URL which names no port

Most URLs do not say which port they mean, so the port of a URL is an `Optional[Int]`. It
cannot be used as a number until something says what its absence means.

```scala fuel
compile
frame
  label The port, used as though it were there
  errors show
  code
      val page = url"https://globe.example.com/plays"

      val port: Int = page.authority.let(_.port)
frame
  label The port, or the default for the scheme
  highlight or(443)
  code
      val page = url"https://globe.example.com/plays"

      val port: Int = page.authority.let(_.port).or(443)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(port.show)
```

[URLs](../../soundness/modules/network-addresses.md#urls) lists the parts of a URL, and
[optionality](../../philosophy/optionality.md) explains `let` and `or`.
