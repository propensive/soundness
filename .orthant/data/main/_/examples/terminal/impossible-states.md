# A key which cannot be pressed

A `Keypress` wraps a key in the modifiers held with it, and each modifier accepts only the keys
it can meaningfully modify. A shifted `a` is the character `A`, so there is no way to write
Shift with a letter.

```scala fuel
compile
frame
  label A shifted letter
  errors show
  code
      val capital = Keypress.Shift(Keypress.CharKey('a'))
frame
  label The letter it produces
  highlight CharKey('A')  the shift has already been applied
  code
      val capital = Keypress.CharKey('A')
frame
  label A key which Shift does change
  highlight Shift(Keypress.Enter)  an editing key keeps its modifier
  code
      val capital = Keypress.CharKey('A')
      val newLine = Keypress.Shift(Keypress.Enter)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(capital.show)
Out.println(newLine.show)
```

[Keyboard input](../../soundness/modules/terminal.md#keyboard-input) lists the keys and their
modifiers, and [impossible states](../../philosophy/impossible-states.md) explains why the wrong
value is left without a spelling.
