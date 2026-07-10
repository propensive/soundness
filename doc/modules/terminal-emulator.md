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
leaves the interactive behaviour untested. What a test wants to check is the *effect*: what the
screen shows, where the cursor is, what color a cell became.

A `Pty` computes that effect, implementing the VT100 and xterm control sequences — cursor
addressing, styling, scroll regions, device reports, hyperlinks — with Unicode handled properly,
wide characters occupying two cells and grapheme clusters kept whole. Everything comes from the
`soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

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

A malformed escape sequence raises a `PtyEscapeError` naming the fault — a test discovers that its
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
terminal state directly.

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
