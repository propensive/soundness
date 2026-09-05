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
import stdios.javaLangSystemStdio
```

A raw-mode session as a scoped block that restores the terminal however it exits is [delimited scopes](../philosophy/delimited-scopes.md) applied to the terminal.

### Styled text

The `e"…"` interpolator styles the spans it marks, with the styles as values and
[colors](colors.md) from the color modules:

```scala
val message = e"$Bold(Warning): ${Fg(WebColors.Red)}(disk full)"
Out.println(message)
```

`Bold`, `Italic`, `Underline` and their siblings mark spans; `Fg` and `Bg` color them.

Colors nest, which raw ANSI does not allow. An escape code sets a color; it cannot restore the one
that was there before, only reset to the default — so a colored region inside another colored
region ends by clearing both. A `Teletype` tracks the style as a stack instead, so the enclosing
color is restored where the inner region ends:

```scala
import WebColors.{Gold, Indigo, HotPink, White}

e"$Gold(gold, $Indigo(indigo, $HotPink(hot pink), indigo) $White(and) gold)"
```

Each substitution is a *transformation* of the prevailing style rather than an absolute setting,
which is what makes that work — and it means a substitution can depend on what it is modifying. A
type becomes usable as one by giving it a `Stylize` instance:

```scala
case object Toggle

given Stylize[Toggle.type] = _ => Stylize(style => style.copy(bold = !style.bold))
```

`$Toggle(…)` then emboldens plain text and unemboldens bold text, deciding from the style it
finds rather than imposing a fixed one.

A `Teletype` behaves as [text](text.md) — it cuts, joins and pads, styles preserved — and renders
per terminal: full 24-bit color on a capable terminal, the nearest palette color on an older one,
and plain text when output is not a terminal at all, so logs never fill with escape codes.

### Keyboard input

`interactive` opens a raw-mode session, within which the terminal's events — keypresses, window
resizes, focus changes, pastes — arrive as typed values:

<!-- doccheck: skip -->
```scala
def handle(char: Char): Unit = ()
def finish(): Unit = ()
def resize(rows: Int, cols: Int): Unit = ()

interactive: terminal ?=>
  terminal.eventIterator().each:
    case Keypress.CharKey(char)  => handle(char)
    case Keypress.Ctrl('C')      => finish()
    case TerminalInfo.WindowSize(rows, cols) => resize(rows, cols)
    case _                       => ()
```

The decoding covers modifier combinations, function keys, the kitty keyboard protocol and
bracketed paste, so `Ctrl(Alt(Left))` is a value to match, not a byte sequence to recognize.

A `Keypress` is an ordinary `CharKey`, a `FunctionKey`, one of the named editing keys — `Tab`,
`Enter`, `Backspace`, `Delete`, `Escape`, `Insert`, `Home`, `End`, `PageUp`, `PageDown` and the
arrows — or an `EscapeSeq` for a sequence with no other name. Each may be wrapped in the modifiers
held with it, nesting outwards:

```scala
Keypress.Ctrl('A')
Keypress.Shift(Keypress.Enter)
```

The wrappers accept only what can meaningfully be modified, so `Shift(CharKey('a'))` does not
typecheck — a shifted `a` is `CharKey('A')`, and the shift has already been applied. A keypress
renders with a Unicode symbol in brackets for each special key, joined to what it modifies with
`+`: `[⇧]+[↵]`, `[⌃]+C`, `[⌥]+[→]`.

The vocabulary is deliberately separate from anything that produces or consumes it: the terminal
decodes escape sequences into these values, and [web automation](web-automation.md) renders them
as the actions a browser understands, without either needing to know about the other.
Optional capabilities — mouse tracking, focus reporting, the alternate screen — switch on by
importing the corresponding `terminalFeatures` given, and are switched off again when the session
ends.

### Line editing and menus

A line of input, with cursor movement and editing keys handled, is a `LineEditor`; a choice among
options is a `SelectMenu`. Each is a pure state machine — an event in, a new state out — asked for
its result in one call:

<!-- doccheck: skip -->
```scala
interactive: terminal ?=>
  LineEditor().ask: text =>
    Out.println(t"You entered: $text")
```

### Layout

Interfaces larger than a line compose from panes: `panel` for content, `editor` and `menu` for the
interactive widgets, `strip` and `stack` to arrange them in columns and rows, and `border` to frame
them. `form` runs the arrangement — full-screen on the alternate buffer, or *inline*, as a live
block at the cursor that leaves scrollback intact:

<!-- doccheck: skip -->
```scala
interactive: terminal ?=>
  val pages = List(t"Home", t"Settings", t"About")
  val sidebar = border(BorderStyle.rounded)(menu(pages, pages.prim.or(t""), maxWidth = 20))
  val body = border(BorderStyle.heavy)(editor(LineEditor()))

  form(Occupancy.Inline)(strip(sidebar, body))
```

Sizes solve as fractions with minima and maxima, and focus moves between widgets with Tab. Live
status — progress bars, spinners, meters and step indicators — embeds the same way; see
[gauges](gauges.md).

### Redrawing without flicker

Rendering keeps a model of what is actually on the screen, and each frame is diffed against it,
so a repaint overprints only the runs that changed and an identical frame emits nothing at all.
Full-screen output is buffered and flushed as one write rather than as a stream of small ones. A
geometry change or a resize takes the full redraw path, because nothing about the previous
contents can then be relied upon.

An inline block — one that leaves scrollback intact — has a further problem: the terminal may
reflow its lines when the window narrows, moving the block out from under the cursor. Rather than
redrawing in the wrong place, an inline block re-establishes where it is after a resize, so a
running interface survives a reflow instead of scribbling over the scrollback above it.
