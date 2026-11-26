# Control characters, and no others

A terminal can report Ctrl with a letter, and with a handful of punctuation marks, because
those are the control characters which ASCII has. The type of `Ctrl` admits exactly those
characters, so a combination which no terminal can send is not a value.

```scala fuel
compile
frame
  label Ctrl with a digit
  errors show
  code
      val interrupt = Keypress.Ctrl('3')
frame
  label Ctrl with a letter
  highlight 'C'  one of the characters which the type lists
  code
      val interrupt = Keypress.Ctrl('C')
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(interrupt.show)
```

[Keyboard input](../../soundness/modules/terminal.md#keyboard-input) describes how the
modifiers nest.
