# A line editor which is a value

A `LineEditor` holds a line of text and the position of the cursor. Applying a keypress to it
gives a new editor, and leaves the old one as it was, so every state of an edit can be kept,
compared and tested without a terminal.

```scala fuel
compile
frame
  label An empty line
  code
      import Keypress.*

      val empty = LineEditor()
frame
  label Two keys typed
  highlight empty(CharKey('O'))(CharKey('h'))  each application is a new editor
  code
      import Keypress.*

      val empty = LineEditor()
      val typed = empty(CharKey('O'))(CharKey('h'))
frame
  label One of them taken back
  highlight typed(Backspace)  typed is unchanged
  code
      import Keypress.*

      val empty = LineEditor()
      val typed = empty(CharKey('O'))(CharKey('h'))
      val corrected = typed(Backspace)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(typed.value.inspect)
Out.println(corrected.value.inspect)
Out.println(typed.value.inspect)
```

[Line editing and menus](../../soundness/modules/terminal.md#line-editing-and-menus) runs the
same state machine against a real keyboard, and [immutability](../../philosophy/immutability.md)
says why it is one.
