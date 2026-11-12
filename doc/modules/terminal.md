## Terminal

### About

An interactive program in a terminal needs three things which the raw console does not give:
styled output, input from the keyboard as events, and a way to lay out and refresh regions of
the screen. All three are provided:

- The `e"…"` [interpolator] writes text with ANSI styles, as a value.
- An `interactive` block puts the terminal in raw mode, and turns keystrokes into a typed
  stream of events.
- A layout layer composes panels, editors and menus into interfaces, full-screen or inline,
  which redraw without flicker.

### On terminals

The terminal is a protocol in which control travels in the same channel as content. Styling,
movement of the cursor and reports from the keyboard all travel as escape sequences, mixed
into the text.

A program which prints escape codes directly ties itself to one terminal's dialect, and
corrupts output which ends up in a file. A program which reads its input a byte at a time
must decode the sequences of several bytes which keys send. And raw mode which is left
enabled after a crash wrecks the user's shell.

Here the protocol is kept at the edges:

- Styled text is a `Teletype`: a value which carries its styles. It is rendered to the
  capabilities of the *actual* terminal when it is output, and to plain text where there is
  no terminal.
- Input arrives as typed events, decoded from the sequences.
- The raw-mode session is a scoped block, which restores the terminal however it exits. That
  is [delimited scopes](../philosophy/delimited-scopes.md), applied to the terminal.

Everything comes from the `soundness` package:

```scala
import soundness.*
import stdios.javaLangSystemStdio
import termcapDefinitions.xtermTrueColorTermcap
```

The second import says where output goes, and the third says what the terminal can display:
here, full 24-bit color.

### Styled text

The `e"…"` interpolator styles the spans which it marks. The styles are values, and the
[colors](colors.md) are colors:

```scala
val message = e"$Bold(Warning): ${Fg(WebColors.Red)}(disk full)"
```

```scala fuel
run
##
Out.println(message)
```

`Bold`, `Italic`, `Underline` and their siblings mark spans, and `Fg` and `Bg` color them.

Colors nest, which raw ANSI does not allow. An escape code sets a color. It cannot restore
the color which was there before, and can only reset to the default, so a colored region
inside another colored region ends by clearing both. A `Teletype` tracks the style as a
stack instead, so the enclosing color is restored where the inner region ends:

```scala fuel
run
##
import WebColors.{Gold, Indigo, HotPink, White}

Out.println(e"$Gold(gold, $Indigo(indigo, $HotPink(hot pink), indigo) $White(and) gold)")
```

Each substitution is a *transformation* of the prevailing style, not an absolute setting,
which is what makes that work. It means that a substitution can depend on what it modifies.
A type becomes usable as one by being given a `Stylize` instance:

```scala
case object Toggle

given toggleStylize: Stylize[Toggle.type] = _ => Stylize(style => style.copy(bold = !style.bold))
```

```scala fuel
run
##
Out.println(e"plain, $Toggle(bold, $Toggle(plain again), bold), plain")
```

`$Toggle(…)` makes plain text bold, and bold text plain. It decides from the style which it
finds, and does not impose a fixed one.

A `Teletype` behaves as [text](text.md#any-textual-type). It is cut, joined and padded, with
its styles preserved. It is rendered for each terminal: in full 24-bit color on a capable
terminal, in the nearest color of the palette on an older one, and as plain text when the
output is not a terminal at all, so that logs never fill with escape codes.
[Environment](environment.md#the-machine) describes how the capabilities of the real terminal
are detected.

### Keyboard input

`interactive` opens a raw-mode session. Within it, the terminal's events arrive as typed
values: keypresses, resizes of the window, changes of focus, and pastes. The session runs
under [supervision](concurrency.md), and takes the console and the
[environment](environment.md) from context. An application's
[`Cli`](cli.md) provides the console:

```scala fuel
typecheck
##
import environments.javaBaseEnvironment
import probates.awaitProbate
import strategies.throwUnsafely
import threads.virtualThreads

given console: Console = Console(summon[Stdio])

def handle(char: Char): Unit = ()
def finish(): Unit = ()
def resize(rows: Int, cols: Int): Unit = ()

supervise:
  interactive: terminal ?=>
    terminal.eventIterator().each:
      case Keypress.CharKey(char)  => handle(char)
      case Keypress.Ctrl('C')      => finish()
      case Terminal.Info.WindowSize(rows, cols) => resize(rows, cols)
      case _                       => ()
```

The decoding covers combinations of modifiers, function keys, the kitty keyboard protocol
and bracketed paste. `Ctrl(Alt(Left))` is therefore a value to match, not a sequence of bytes
to recognize.

A `Keypress` is an ordinary `CharKey`, a `FunctionKey`, one of the named editing keys, or an
`EscapeSeq` for a sequence with no other name. The named keys are `Tab`, `Enter`,
`Backspace`, `Delete`, `Escape`, `Insert`, `Home`, `End`, `PageUp`, `PageDown` and the
arrows. Each may be wrapped in the modifiers which were held with it, nesting outwards:

```scala
val selectAll = Keypress.Ctrl('A')
val newLine = Keypress.Shift(Keypress.Enter)
```

The wrappers accept only what can meaningfully be modified. `Shift(CharKey('a'))` does not
typecheck: a shifted `a` is `CharKey('A')`, and the shift has already been applied.

A keypress is displayed with a Unicode symbol in brackets for each special key, joined with
`+` to what it modifies, as in `[⇧]+[↵]`, `[⌃]+[C]` or `[⌥]+[→]`:

```tabular
column  Keypress
column  Renders as
row  `CharKey('a')`  `[a]`
row  `CharKey(' ')`  `[␣]`
row  `FunctionKey(5)`  `[F5]`
row  `Tab`, `Enter`  `[⇥]`, `[↵]`
row  `Backspace`, `Delete`, `Insert`  `[⌫]`, `[⌦]`, `[⎀]`
row  `Escape`  `[⎋]`
row  `Up`, `Down`, `Left`, `Right`  `[↑]`, `[↓]`, `[←]`, `[→]`
row  `Home`, `End`  `[↖]`, `[↘]`
row  `PageUp`, `PageDown`  `[⇞]`, `[⇟]`
row  `EscapeSeq(id, …)`  `[⎋id]`
row  `Shift(…)`, `Ctrl(…)`, `Alt(…)`, `Meta(…)`  `[⇧]+…`, `[⌃]+…`, `[⌥]+…`, `[⌘]+…`
```

The vocabulary is deliberately separate from anything which produces or consumes it. The
terminal decodes escape sequences into these values, and
[web automation](web-automation.md) turns them into the actions which a browser understands,
without either needing to know about the other.

Optional capabilities, such as tracking of the mouse, reporting of focus and the alternate
screen, are switched on by importing the corresponding given from `terminalFeatures`, and
are switched off again when the session ends.

### Line editing and menus

A line of input, with movement of the cursor and the editing keys handled, is a
`LineEditor`. A choice among options is a `SelectMenu`. Each is a pure state machine, which
takes an event and gives a new state, and is asked for its result in one call:

```scala fuel
typecheck
##
supervise:
  interactive: terminal ?=>
    LineEditor().ask: text =>
      Out.println(t"You entered: $text")
```

### Layout

Interfaces which are larger than a line are composed from panes:

```tabular
column  Combinator
column  Makes
row  `panel`  a pane of content
row  `editor`, `menu`  an interactive widget: a `LineEditor` or a menu of options
row  `strip`  panes side by side, as columns
row  `stack`  panes one above another, as rows
row  `border`  a frame around a pane
```

`conduct` runs the arrangement. It runs full-screen, on the alternate buffer, or *inline*, as
a live block at the cursor, which leaves the scrollback intact:

```scala fuel
typecheck
##
import ultimatum.border

supervise:
  interactive: terminal ?=>
    val pages = List(t"Home", t"Settings", t"About")
    val sidebar = border(BorderStyle.rounded)(menu(pages, pages.head.or(t""), maxWidth = 20))
    val body = border(BorderStyle.heavy)(editor(LineEditor()))

    conduct(Tui.Occupancy.Inline)(strip(sidebar, body))
```

Sizes are solved as fractions, with minima and maxima, and focus moves between widgets with
Tab. Live status is embedded in the same way: progress bars, spinners, meters and step
indicators. [Gauges](gauges.md) describes them.

### Redrawing without flicker

Rendering keeps a model of what is actually on the screen, and each frame is compared with
it. A repaint therefore overprints only the runs which changed, and an identical frame emits
nothing at all. Full-screen output is buffered, and flushed as one write, not as a stream of
small ones. A change of geometry, or a resize, takes the path of a full redraw, because
nothing about the previous contents can then be relied upon.

An inline block, which leaves the scrollback intact, has a further problem. The terminal may
reflow its lines when the window narrows, moving the block out from under the cursor. An
inline block therefore works out again where it is after a resize, so that a running
interface survives a reflow, and does not scribble over the scrollback above it.

### Related

- [Colors](colors.md) describes the colors which `Fg` and `Bg` take.
- [Tables](tables.md) and [Gauges](gauges.md) draw structured output in a terminal.
- [Command-line interfaces](cli.md) reads a program's arguments and completes them.
- [Terminal emulator](terminal-emulator.md) is the other side: it interprets the escape
  sequences which a program writes.
- [Processes](processes.md#on-a-terminal) runs another program on a pseudo-terminal.
