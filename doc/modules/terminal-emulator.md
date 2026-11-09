## Terminal Emulation

### About

Testing a terminal application means checking what a terminal *would show*, and that
requires a terminal. A `Pty` is one, with no screen. It consumes text which is laden with
ANSI escape sequences, and maintains the state which a real terminal would: a grid of
characters with their styles, the cursor, the title, and the scrolling region. That state is
an [immutable] value, which a test can inspect a cell at a time.

### On terminal emulation

The output of a terminal application is not text. It is a program in the terminal's control
language: movements of the cursor, changes of style, and clearings of the screen, mixed in
with characters. Asserting on that stream of bytes directly is brittle, because many
sequences produce the same screen. Asserting on nothing leaves the interactive behavior
untested. What a test wants to check is the *effect*: what the screen shows, where the
cursor is, and what color a cell became.

A `Pty` computes that effect. It implements the control sequences of VT100 and xterm:
addressing of the cursor, styling, scrolling regions, device reports and hyperlinks. Unicode
is handled properly, with wide characters occupying two cells, and grapheme clusters kept
whole. A screen which is an immutable value, transformed by each escape sequence, is
[immutability](../philosophy/immutability.md) applied to something which is usually mutated
in place.

Everything comes from the `soundness` package. The samples throw any error as an exception,
and the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Feeding a terminal

A `Pty` is created at a size, as a width and then a height. It consumes input, and gives the
new state of the terminal. The value is immutable, so intermediate states can be kept and
compared:

```scala
val pty = Pty(80, 24)

val after = pty.consume(t"hello")
```

Escape sequences in the input do what they would do on a screen. This one moves the cursor
to row 3 and column 5, and then writes an `X`:

```scala
val esc = 0x1b.toChar
val moved = pty.consume(t"$esc[3;5HX")
```

A malformed escape sequence raises a `Pty.Error`, which names the fault. A test therefore
discovers that its application emits broken sequences, where a real terminal would quietly
mangle them.

### Inspecting the screen

The screen buffer answers for each cell: its character, its style, including bold, italic
and colors, and any hyperlink which is attached to it. A cell is addressed by its column and
its row, as [ordinals](numbers.md#ordinals):

```scala
val styled = pty.consume(t"$esc[38;2;100;150;200mX")
```

```scala fuel
run
##
Out.println(after.buffer.char(Prim, Prim).inspect)
Out.println(moved.buffer.char(Quin, Ter).inspect)
Out.println((styled.buffer.style(Prim, Prim).foreground == rgb"#6496c8").inspect)
```

The last line confirms that the cell's foreground is the color which the sequence asked for,
with red, green and blue of 100, 150 and 200.

The position of the cursor, whether it is visible, and the window's title, as the
application set it, are read from the state of the terminal directly:

```scala fuel
run
##
val titled = pty.consume(t"$esc]0;My application${7.toChar}$esc[?25l")

Out.println(titled.state.title.inspect)
Out.println(titled.state.hideCursor.inspect)
```

The window's title arrives through an operating-system-command sequence, which is terminated
either by the bell character or by a string terminator. Both spellings are accepted, since
applications use both.

### What is understood

The emulator covers the vocabulary which a real application uses:

```tabular
column  Sequence
column  Effect
row  CR, LF, BS, FF  carriage return, line feed, backspace and form feed, as a terminal's
row  HT  advances to the next eight-column tab stop
row  other C0 controls  ignored
row  `CSI A` to `CSI G`, `CSI d`, `CSI H`, `CSI f`  move and position the cursor
row  `ESC D`, `ESC E`, `ESC M`  index, next line and reverse index
row  `CSI J`, `CSI K`, `CSI X`  erase the screen, the line, or characters; `ED 2` clears the screen and `EL 2` the line
row  `CSI @`, `CSI P`, `CSI L`, `CSI M`, `CSI b`  insert and delete characters and lines, and repeat a character
row  `CSI S`, `CSI T`, `CSI r`  scroll up and down, and set the scrolling region
row  `ESC 7`, `ESC 8`, `CSI s`, `CSI u`  save and restore the cursor, as `DECSC` and `DECRC`
row  `CSI m`  `SGR` styling
row  `CSI ?25 h`, `CSI ?25 l`  show and hide the cursor
row  `CSI ?1049`, `CSI ?1047`, `CSI ?47`  enter and leave the alternate screen
row  `CSI ?1004`, `CSI ?2004`  focus detection and bracketed paste modes
row  other DEC private modes, ANSI modes, `CSI t`  ignored
row  `CSI 6n`, `CSI c`, `CSI >c`  answered with a report
row  `OSC 0`, `OSC 2`  set the window title
row  `OSC 8`  attaches a hyperlink to the cells written
row  `DCS`, `APC`, `PM`, `SOS`  absorbed up to their terminator
row  `ESC c`  `RIS`: resets the terminal to its initial state
row  character set designations, `SS2`, `SS3`  ignored: only the default character set is supported
```

Styling through `SGR` covers the attribute flags, the sixteen colors of the palette, with
their bright variants as distinct entries, the palette of 256 colors, and 24-bit color.
Resetting with `SGR 0` affects the cells which are written afterwards, not those already on
the screen, exactly as in a terminal.

DEC private modes are honored where they change what a test would observe, and are silently
ignored where they do not, so an application is not derailed by an emulator which refuses a
sequence. Sequences which carry arbitrary payloads are absorbed, not interpreted, which is
what a terminal that does not implement them does. `RIS` lets a test start a fresh scenario
without building a new emulator.

### Reports

Some sequences make a terminal *answer*: a report of the cursor's position, or a query for
device attributes. A `Pty` produces those answers on its output stream. An application which
interrogates its terminal can therefore be tested against the responses which it would
really receive:

```scala
val queried = pty.consume(t"$esc[6n")
val report = queried.stream
```

The sequence asks for the position of the cursor, and the stream carries the report, in the
form `ESC[row;colR`.

This is the machinery behind Soundness's own terminal tests. A [terminal](terminal.md)
interface runs against a `Pty`, and assertions read the screen.

### Related

- [Terminal](terminal.md) writes the escape sequences which a `Pty` interprets.
- [Testing](testing.md) runs the assertions.
- [Processes](processes.md#on-a-terminal) runs a real program on a pseudo-terminal of the
  operating system.
