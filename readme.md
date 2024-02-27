[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/profanity/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/profanity/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Profanity

__A library for realtime interactive terminal software__

_Profanity_ makes it possible to write real-time applications that interact through the terminal by
converting STDIN into an event stream of keypresses.

## Features

- capture a TTY terminal's individual keypresses
- interprets standard keys and control keys
- simple interactive line editor supporting standard keypresses
- simple interactive navigable menu


## Availability Plan

Profanity has not yet been published. The medium-term plan is to build Profanity
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Profanity.

Subsequently, Profanity will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

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





## Status

Profanity is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Profanity is designed to be _small_. Its entire source code currently consists
of 313 lines of code.

## Building

Profanity will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Profanity?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Profanity's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Profanity and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `profanity`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Profanity's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Profanity are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/profanity/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Profanity
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Profanity was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A __profanity__ is an expletive or curse-word, and _Profanity_ imitates many of the features of the popular terminal library, Curses.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows the horns of a devil; the epitome of the profane.

## License

Profanity is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

