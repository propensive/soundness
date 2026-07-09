## Terminal

### About

Interactive terminal programs need three things the raw console does not give: styled output,
keyboard input as events, and a way to lay out and refresh regions of the screen. Soundness
provides all three. The `e"…"` interpolator writes ANSI-styled text as a value; an `interactive`
block puts the terminal in raw mode and turns keystrokes into a typed event stream; and a layout
layer composes panels, editors and menus into full-screen or inline interfaces that redraw without
flicker.

### On terminals

The terminal is an in-band protocol: styling, cursor movement and keyboard reports all travel as
escape sequences mixed into the text. Programs that print escape codes directly tie themselves to
one terminal's dialect and corrupt output that ends up in a file; programs that read stdin byte by
byte must decode multi-byte key sequences themselves, and raw mode left enabled on a crash wrecks
the user's shell.

Soundness keeps the protocol at the edges. Styled text is a `Teletype` — a value carrying its
styles, rendered to the *actual* terminal's capabilities at output, and to plain text where there
is no terminal. Input arrives as typed events, decoded from the sequences. And the raw-mode
session is a scoped block that restores the terminal however it exits. Everything comes from the
`soundness` package:

```scala
import soundness.*
```

### Styled text

The `e"…"` interpolator styles the spans it marks, with the styles as values and
[colors](colors.md) from the color modules:

```scala
val message = e"$Bold(Warning): ${Fg(WebColors.Red)}(disk full)"
Out.println(message)
```

`Bold`, `Italic`, `Underline` and their siblings mark spans; `Fg` and `Bg` color them. A
`Teletype` behaves as [text](text.md) — it cuts, joins and pads, styles preserved — and renders
per terminal: full 24-bit color on a capable terminal, the nearest palette color on an older one,
and plain text when output is not a terminal at all, so logs never fill with escape codes.

### Keyboard input

`interactive` opens a raw-mode session, within which the terminal's events — keypresses, window
resizes, focus changes, pastes — arrive as typed values:

```scala
interactive: terminal ?=>
  terminal.eventIterator().each:
    case Keypress.CharKey(char)  => handle(char)
    case Keypress.Ctrl('C')      => finish()
    case TerminalInfo.WindowSize(rows, cols) => resize(rows, cols)
    case _                       => ()
```

The decoding covers modifier combinations, function keys, the kitty keyboard protocol and
bracketed paste, so `Ctrl(Alt(Left))` is a value to match, not a byte sequence to recognise.
Optional capabilities — mouse tracking, focus reporting, the alternate screen — switch on by
importing the corresponding `terminalFeatures` given, and are switched off again when the session
ends.

### Line editing and menus

A line of input, with cursor movement and editing keys handled, is a `LineEditor`; a choice among
options is a `SelectMenu`. Each is a pure state machine — an event in, a new state out — asked for
its result in one call:

```scala
interactive: terminal ?=>
  LineEditor().ask: text =>
    Out.println(t"You entered: $text")
```

### Layout

Interfaces larger than a line compose from panes: `panel` for content, `editor` and `menu` for the
interactive widgets, `file` and `rank` to arrange them in columns and rows, and `border` to frame
them. `form` runs the arrangement — full-screen on the alternate buffer, or *inline*, as a live
block at the cursor that leaves scrollback intact:

```scala
interactive: terminal ?=>
  val sidebar = border(BorderStyle.rounded)(menu(pages, pages.head, maxWidth = 20))
  val body = border(BorderStyle.heavy)(editor(LineEditor()))

  form(Mode.Inline)(file(sidebar, body))
```

Sizes solve as fractions with minima and maxima, focus moves between widgets with Tab, and
repaints touch only the cells that changed.
