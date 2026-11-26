# A method which knows where it was called

A method which reports a position in the source asks for a `Codepoint` as a contextual
parameter. The compiler supplies the file and line of each call, so no caller passes a location,
and the signature shows that the method depends on one.

```scala fuel
compile
frame
  label A cue, with no position
  code
      def cue(line: Text): Text = t"cue: $line"

      val entrance: Text = cue(t"Enter Ghost")
frame
  label The position of the call is asked for
  highlight (using..Codepoint)  filled in by the compiler at each call
  code
      def cue(line: Text)(using codepoint: Codepoint): Text =
        t"line ${codepoint.line}: $line"

      val entrance: Text = cue(t"Enter Ghost")
frame
  label Two calls, two positions
  highlight cue(t"Exit  a line below the first
  code
      def cue(line: Text)(using codepoint: Codepoint): Text =
        t"line ${codepoint.line}: $line"

      val entrance: Text = cue(t"Enter Ghost")
      val exit: Text = cue(t"Exit Ghost")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(entrance)
Out.println(exit)
```

[Codepoints](../../soundness/modules/stack-traces.md#codepoints) names the libraries which use
one, and [honest signatures](../../philosophy/honest-signatures.md) the principle.
