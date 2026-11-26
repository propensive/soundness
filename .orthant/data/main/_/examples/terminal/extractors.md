# Keys taken apart in a pattern

A keypress is a value, and the modifiers held with it are wrappers around the key. A `match`
on keypresses therefore reads as the keys do, and never mentions an escape sequence.

```scala fuel
compile
frame
  label An ordinary character
  code
      import Keypress.*

      def describe(key: Keypress): Text = key match
        case CharKey(char) => t"typed $char"
        case _             => t"ignored"
frame
  label A control character
  highlight Ctrl('C')  a literal, matched exactly
  code
      import Keypress.*

      def describe(key: Keypress): Text = key match
        case CharKey(char) => t"typed $char"
        case Ctrl('C')     => t"interrupted"
        case _             => t"ignored"
frame
  label Two modifiers and an arrow
  highlight Ctrl(Alt(Left))  nesting outwards from the key
  code
      import Keypress.*

      def describe(key: Keypress): Text = key match
        case CharKey(char)   => t"typed $char"
        case Ctrl('C')       => t"interrupted"
        case Ctrl(Alt(Left)) => t"back a word"
        case _               => t"ignored"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(describe(CharKey('x')))
Out.println(describe(Ctrl('C')))
Out.println(describe(Ctrl(Alt(Left))))
Out.println(describe(Shift(Tab)))
```

[Keyboard input](../../soundness/modules/terminal.md#keyboard-input) shows the same patterns
applied to the events of a live terminal.
