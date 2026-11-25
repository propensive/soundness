# One message, three terminals

Styled text is a value which carries its styles, and holds no escape codes. What is written
when it is printed is decided by the terminal's capabilities, and those are an import.

```scala fuel
compile
alternative A file
  highlight basicTermcap  plain text, with no escape codes
  code
      import stdios.javaLangSystemStdio
      import termcapDefinitions.basicTermcap

      val warning = e"$Bold(Warning): ${Fg(WebColors.Crimson)}(the storm is up)"

      def report(): Unit = Out.println(warning)
alternative 256 colors
  highlight xterm256Termcap  the nearest color of the palette
  code
      import stdios.javaLangSystemStdio
      import termcapDefinitions.xterm256Termcap

      val warning = e"$Bold(Warning): ${Fg(WebColors.Crimson)}(the storm is up)"

      def report(): Unit = Out.println(warning)
alternative True color
  highlight xtermTrueColorTermcap  crimson, exactly
  code
      import stdios.javaLangSystemStdio
      import termcapDefinitions.xtermTrueColorTermcap

      val warning = e"$Bold(Warning): ${Fg(WebColors.Crimson)}(the storm is up)"

      def report(): Unit = Out.println(warning)
##
```

The message and the code which prints it are the same in each.
[Styled text](../../soundness/modules/terminal.md#styled-text) describes the rendering, and
[declarative context](../../philosophy/declarative-context.md) the reason for choosing it by
import.
