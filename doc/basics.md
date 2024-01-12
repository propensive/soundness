Java does not provide native support for direct access to keypress events. While Standard input (`STDIN`)
is accessible as an input stream, it is buffered until a newline is sent, which makes it impossible for a
Scala application to respond immediately to a keypress event, unless that keypress is the `Enter` key.

Profanity uses the Java Native Interface (JNI) to turn off buffering so that each keypress is received as
soon as it happens.

### Capturing the TTY

Before keypresses can be streamed as events, the TTY must be "captured". This is as simple as,
```scala
Tty.capture {
  // TTY operations are available here
}
```
but may not always succeed, for example if the JVM is not running inside a TTY, or if the TTY has already
been captured, or if the JNI calls fail for another reason. These exceptions are checked.

### Streaming keypresses

Within a `Tty.capture` block, a contextual `Tty` instance is made available, and `Tty.stream` can be
called which will, by default, return a `LazyList[Keypress]`, where `Keypress` is Profanity's standard
representation of a keypress event. `Keypress` is an enumeration providing the following cases:
- `Printable(c: Char)`, a keypress of a printable character, for example, `Shift+T` is `Printable('T')`
- `Function(i: Int)`, a function key keypress, where `i` is the function key number, for example `F2` is `Function(2)`
- `Ctrl(c: Char)`, a key combination of `Ctrl` and another character, for example, `Ctrl+C` is `Ctrl('c')`
- a keypress of one of the following keys: `Enter`, `Escape`, `Tab`, `Backspace`, `Delete`, `PageUp`,
  `PageDown`, `LeftArrow`, `RightArrow`, `UpArrow`, `DownArrow`, `CtrlLeftArrow`, `CtrlRightArrow`,
  `CtrlUpArrow`, `CtrlDownArrow`, `End`, `Home`, `Insert`
- `Escape(bytes: Byte*)`, any other escape sequence that hasn't been identified as one of the above

Additionally, the `Resize(rows: Int, cols: Int)` case represents the escape sequence that reports
the width and height of the console, and may be triggered by calling the `Tty.reportSize()`, or when a
`SIGWINCH` event occurs, i.e. when the terminal window's size changes. So a `Resize` event can be handled
just like a keypress event, where an action make be taken to ignore the event or redraw the screen, or
something else.

#### Alternative keyboards

`Tty.stream` takes an optional type parameter, `K`, which determines the type used to represent keypress events,
by resolving a contextual `Keyboard[K]` instance that interprets the bytes arriving in `STDIN` (either
individually, or as short sequences) as instances of `K`.

By default, only a single `Keyboard` given instance is defined, parameterized on the event type, `Keypress`,
which means that `Tty.stream` may be invoked without specifying its type parameter. Nevertheless, it is
possible to define alternative interpreters for the byte input to `STDIN`.

### Line Editor

Profanity provides a simple line editor that may be used inside a `Tty.capture` block, and will handle
common keypresses that may be used inside an editable field, including printable characters, arrow keys,
`Ctrl` key combinations such as `Ctrl+W` (delete word) and `Ctrl+U` (delete line).

This can be invoked inside a `Tty.capture` block with `LineEditor.ask()`, or pre-filled with an initial
value, `LineEditor.ask("initial")`.

#### Rendering

It is also possible to control how the line editor displays the text by overriding the default `render`
method of `LineEditor.ask`. This parameter is a `String => String` lambda, mapping from the current
value (i.e. the accumulation of several keypresses into a string) to the value that should be printed.

While `render` would normally use the identity function, it is possible to use the `LineEditor.concealed`
method to display each character as an `*` or even to include ANSI escape characters (e.g. from
[Escapade](https://github.com/propensive/escapade)) in the string. It is executed, and the line is
redrawn, for every keypress.

### Menus

A simple menu of two or more options is provided through the `SelectMenu` object. Its `ask` method,
which can only be called inside a `Tty.capture` block, will present a set of options to the user, of
which exactly one must be chosen using the arrow keys and the `Enter` key. In addition to the list of
choices being supplied as the first parameter of `SelectMenu.ask`, the initially-selected value may be
provided as the second.

Additionally, two lambdas, `renderOn` and `renderOff`, allow the rendering of each menu item (whether
_on_ or _off_) to be specified.

### Limitations

Profanity does not currently support Windows.



