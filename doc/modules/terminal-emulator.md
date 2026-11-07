## Terminal Emulation

### About

Testing a terminal application means checking what a terminal *would show* — and that requires a
terminal. A `Pty` is one, headless: it consumes text laden with ANSI escape sequences and maintains
the state a real terminal would — a grid of characters with their styles, the cursor, the title,
the scroll region — as an immutable value that a test can inspect cell by cell.

### On terminal emulation

A terminal application's output is not text but a program in the terminal's control language:
cursor moves, style changes, screen clears interleaved with characters. Asserting on that byte
stream directly is brittle — many sequences produce the same screen — and asserting on nothing
leaves the interactive behavior untested. What a test wants to check is the *effect*: what the
screen shows, where the cursor is, what color a cell became.

A `Pty` computes that effect, implementing the VT100 and xterm control sequences — cursor
addressing, styling, scroll regions, device reports, hyperlinks — with Unicode handled properly,
wide characters occupying two cells and grapheme clusters kept whole. Everything comes from the
`soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

A screen as an immutable value that each escape sequence transforms is [immutability](../philosophy/immutability.md) applied to something usually mutated in place.

### Feeding a terminal

A `Pty` is created at a size — width, then height — and consumes input, returning the new terminal
state; the value is immutable, so intermediate states can be kept and compared:

```scala
val pty = Pty(80, 24)

val after = pty.consume(t"hello")
```

Escape sequences in the input do what they would on screen:

```scala
val esc = 0x1b.toChar
val moved = pty.consume(t"$esc[3;5HX")   // cursor to row 3, column 5, then an X
```

A malformed escape sequence raises a `Pty.Error` naming the fault — a test discovers that its
application emits broken sequences, rather than a real terminal quietly mangling them.

### Inspecting the screen

The screen buffer answers by cell: the character, its style — bold, italic, colors, and the rest —
and any hyperlink attached to it:

```scala
after.buffer.char(Prim, Prim)              // 'h' — the top-left cell
moved.buffer.char(Quin, Ter)               // 'X'

val styled = pty.consume(t"$esc[38;2;100;150;200mX")
styled.buffer.style(Prim, Prim).foreground   // Chroma(100, 150, 200)
```

The cursor position, its visibility, and the window title set by the application are read from the
terminal state directly:

```scala
pty.state.cursor       // where the cursor sits
pty.state.hideCursor   // whether DECTCEM hid it
pty.state.title        // the title set by an OSC sequence
```

The window title arrives through an operating-system-command sequence, terminated by either the
bell character or a string terminator, and both spellings are accepted, since applications use
both.

### What is understood

The emulator covers the vocabulary a real application uses:

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

Styling through `SGR` covers the attribute flags, the sixteen palette colors with their bright
variants as distinct entries, the 256-color palette, and 24-bit color. Resetting with `SGR 0`
affects the cells written afterwards, not those already on screen, exactly as a terminal does.

DEC private modes are honoured where they change what a test would observe and silently ignored
where they do not, so an application is not derailed by an emulator that refuses a sequence.
Sequences carrying arbitrary payloads are absorbed rather than interpreted, which is what a
terminal that does not implement them does. `RIS` lets a test start a fresh scenario without
building a new emulator.

### Reports

Some sequences make a terminal *answer* — a cursor-position report, a device attributes query —
and a `Pty` produces those answers on its output stream, so an application that interrogates its
terminal can be tested against the responses it would really receive:

```scala
val queried = pty.consume(t"$esc[6n")   // ask for the cursor position
queried.stream                          // carries the ESC[row;colR report
```

This is the machinery behind Soundness's own terminal tests: a [terminal](terminal.md) interface
runs against a `Pty`, and assertions read the screen.
