All terms and types are defined in the `yossarian` package:
```scala
import yossarian.*
```

### Create a new Pseudo-Terminal

An instance of `Pty`, representing a pseudo-terminal (PTY), is the main
entry-point to Yossarian's features. We can create one by specifying its width
and height:
```scala
val pty = Pty(80, 24)
```

This represents a screen size of 80×24 characters, white-on-black text, with a
cursor in the top-left corner, and no interesting text styles.

### Updating PTY state

Input may be provided to the `Pty` instance by supplying it as a `Text` value
to the `consume` method. For example,
```scala
val pty2 = pty.consume(t"Hello world\n")
```

This will construct a new `Pty` with the words `Hello world` on its virtual
screen, and move the cursor to the start of the next line. This is an immutable
operation, so the original state of `pty` will be unchanged.

### Accessing PTY state

Changing the state of the PTY is not useful unless we can inspect its state! We
can access much of that state through `pty.buffer`, and instance of
`ScreenBuffer`.

A `ScreenBuffer` represents a rectangular region of characters in the
pseudo-terminal, which may not be the entire terminal window, and provides the
following methods:
 - `width` and `height`, to get the buffer's dimensions
 - `char(x, y)`, to get the character at a particular position
 - `style(x, y)`, to get the `Style` instance for a position
 - `link(x, y)`, to get the link text applied to the position
 - `line`, to get a new `ScreenBuffer` of the entire screen as a single line
 - `render`, to get the textual content of the screen
 - `styles`, to get an array of the `Style`s for each character in the buffer
 - `find(text)`, to get a smaller `ScreenBuffer` whose content matches the
   search text

A `Style` value provides information on the visual style of a single character
in a `ScreenBuffer`. It includes the properties, `bold`, `italic`, `blink`
(blinking text), `faint`, `conceal` (invisible text), `strike`
(strike-through), `underline`, `reverse` (inverted colors), `foreground`
(color) and `background` (color).

#### Interactivity

A pseudoterminal sometimes needs to _produce_ output to communicate with the
process controlling it. For example, if the PTY receives the escape codes to
query its size, it needs to respond through some channel.

The `stream` method of `Pty` will provide a stream of `Text` output from the
pseudo-terminal, which the controlling process and read and interpret
accordingly.


