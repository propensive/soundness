[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/tarantula/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/tarantula/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Tarantula

__Drive a web browser using the WebDriver protocol__

_Tarantula_ makes it possible to interact with a web browser through a programmatic interface. It
provides an immutable API for controlling the web browser from Scala, through the WebDriver
protocol.

## Features

- simulate keypresses, chords, clicks and drags in a web browser
- automatically launch Chrome, Firefox, Safari or Edge, headless or not
- or drive a driver already running, including a remote grid
- uses the standard W3C WebDriver protocol
- the session is a capability, confined to its block by capture checking
- element locators are typed: CSS selectors, HTML tags, DOM ids and class lists
- every failure is a typed `WebDriver.Error`, not an exception

## Availability







## Getting Started

### Sessions

_Tarantula_ makes it possible to control a web browser programmatically from Scala, over the
W3C [WebDriver](https://www.w3.org/TR/webdriver2/) protocol. Chrome, Firefox, Safari and Edge
are supported, through their standard driver executables.

Every browser operation happens inside a _session_. A session launches the driver, asks it for
a browser, lends it to a block, and stops both when the block ends — whether it returns or
throws:
```scala
WebDriver.Firefox.session:
  browser.navigateTo(url"https://example.com/")
```

The value being driven, `WebDriver.Firefox`, is a description rather than anything live: it may
be a `val`, shared between tests, and used for several sessions in turn. It can be adjusted
before a session is opened:
```scala
val browser = WebDriver.Chrome.headless.on(9515)
```
`headless` runs the browser without a window — `--headless=new` for Chrome and Edge, `-headless`
for Firefox — and `on` chooses the port the driver listens on. `arguing(…)` passes further
arguments to the browser, and `requesting(json)` replaces the generated
[capabilities](https://www.w3.org/TR/webdriver2/#capabilities) entirely, for the cases this API
does not model: proxies, mobile emulation, or a grid's own extensions.

A driver that is already running — started by hand, or a remote [Selenium
grid](https://www.selenium.dev/documentation/grid/) — is driven the same way, given its address:
```scala
WebDriver(url"http://localhost:4444", capabilities).session:
  browser.title()
```

The session handle is a capability, confined by capture checking to the block that owns it.
Returning it, or anything that captures it, is a compile error: a session which has been closed
is not something a program should be able to hold.

The session also knows what the driver actually agreed to, which is not the same as what was
asked for: `browserName`, `browserVersion` and `platformName` name what answered, and
`capabilities` is the whole negotiated object, vendor keys included.

### Simple navigation

Inside the block, `browser` is the session, and navigates:

- `navigateTo(url)` — send the browser to a URL
- `refresh()`, `back()`, `forward()` — the browser's own controls
- `title()`, `source()` — the page's title and its serialized HTML
- `url()` — the current URL

### Finding elements

`element` finds one element and `/` finds every match, each taking a typed way of locating it:

- `Text` — by link text
- `SelectorList`, from [Cataclysm](https://github.com/propensive/cataclysm) — by CSS selector
- an HTML tag, from [Honeycomb](https://github.com/propensive/honeycomb) — by tag name
- `Name[DomId]` — by DOM id
- `ClassList` — by CSS class
- `XPath`, from [Xylophone](https://github.com/propensive/xylophone) — by XPath

So the link reading `here` is `browser.element(t"here")`, and the first image on the page is
`browser.element(Img)`. Both return a `WebDriver.Element`: a handle to a node in the live page, not a
copy of it, so every question about it is a round trip to the browser. An element that does not
exist raises `WebDriver.Error` with reason `NoSuchElement`.

HTML is a tree, so elements are found within elements:
```scala
val link = browser.element(Nav).element(Name[DomId](t"menu")).element(t"About")
```

`/` is defined on the session, on `WebDriver.Element` and on `List[WebDriver.Element]`, and always returns a
`List[WebDriver.Element]` — so a selection can be narrowed by repeated application:
```scala
for element <- browser / Name[DomId](t"menu") / Li / ClassList["checkbox"]()
do element.click()
```

An open shadow root is reached with `shadowRoot()`, and searched with the same `element` and `/`.

Note that `XPath` currently expresses only absolute paths of element steps with 1-indexed
ordinals, plus a trailing attribute — `xp"/html[1]/body[1]/div[2]"`. The predicate forms XPath is
usually reached for cannot be written yet; they will work here unchanged once Xylophone's XPath
supports them.

### Using elements

An element can be acted on:
```scala
field.value(t"search terms")   // type into it
button.click()
field.clear()
```

and interrogated. Note that `attribute` reads the markup's attribute while `property` reads the
live DOM property; the two diverge as soon as a page's script touches the node.
```scala
heading.innerText()            // the rendered text
input.property(t"value")       // what the user would see in the field
input.attribute(t"value")      // what the HTML said
element.tagName()
element.css(t"display")
element.rect()                 // its bounding box
element.enabled()
element.selected()
element.displayed()
element.role()                 // its computed ARIA role
```

`screenshotData()` returns the raw PNG bytes of an element, and of the viewport when called on
the session. The `tarantula.image` component adds `screenshot()`, which decodes them into a
`Raster in Png`; it is separate so that browser automation does not drag in an image-codec
library.

### Input actions

Text typed into a field should go through `value`, which is simpler and faster. Everything else
— a modifier, a chord, a hover, a drag — goes through the actions API:
```scala
browser.press(Keypress.Ctrl('A'), Keypress.Enter)
browser.releaseActions()
```
`Keypress` comes from [Clavichord](https://github.com/propensive/clavichord), the same vocabulary
terminal input uses, so a modifier is written as a wrapper rather than as a codepoint to look up.
Each keypress becomes the actions that produce it: the modifiers pressed, the key struck and
released, then the modifiers released in reverse — which is the only way the protocol can express
a chord. `releaseActions()` lets go of anything an earlier `press` left held.

For anything `press` does not cover — pointer movement, drags, precise pauses — the underlying
actions API is there:
```scala
import WebDriver.Session.Action.*

browser.perform(Source.Pointer, List(PointerMove(120, 40), PointerDown(), PointerUp()))
```

### Waiting

The protocol's `implicit` timeout covers find commands, and can be set once for the session:
```scala
browser.timeouts(WebDriver.Session.Timeouts(`implicit` = 5000L))
```

Nothing else has a server-side notion of waiting, so for a button becoming enabled or a heading's
text changing there is `awaitElement`, `awaitElements` and `awaitUntil`, which poll under the
ambient retry policy:
```scala
import retryTenacities.exponentialTenTimesTenacity

val result = browser.awaitElement(Name[DomId](t"result"))
browser.awaitUntil(browser.title() == t"Done")
```
The policy is a `Tenacity` — `parasite`'s pure retry schedules, exported from `soundness` — so
the interval and the number of attempts are the caller's to choose, and giving up raises
`RetryError`. These poll `/elements`, which reports absence as an empty list rather than as an
error; that matters because a session's error strategy is fixed when it opens, so a failure
raised inside the block cannot be caught here to drive a retry.

### Windows, frames and prompts

`window()` gives the current window's handle and `windows()` all of them; `newWindow()` opens
one and returns its handle without switching to it, and `switchTo` moves between them. Frames
are entered with `frame(element)` or `frame(index)`, and left with `parentFrame()` or
`topFrame()`.

A user prompt blocks every other command until it is dealt with, so `alertText()`,
`acceptAlert()` and `dismissAlert()` handle it. `cookies()`, `addCookie`, `deleteCookie` and
`deleteCookies()` manage the cookie jar.

### Printing

`printPage()` renders the page to PDF, returning the raw bytes. Its options are typed, and the
lengths are quantities rather than bare numbers — the protocol wants centimetres, and the
conversion is computed rather than assumed:
```scala
browser.printPage:
  WebDriver.Session.Print
    ( orientation = WebDriver.Session.Orientation.Landscape,
      margin      = WebDriver.Session.Margin.uniform(15.0*Milli(Metre)),
      background  = true )
```
Not every driver implements it; Safari does not.

### Running scripts

`execute` runs JavaScript in the page and returns its result as `Json`; `executeAsync` runs a
script that calls a completion callback. This is the escape hatch for anything the protocol
does not model.

### Errors

Every failure raises `WebDriver.Error`, carrying the reason the driver reported (one of the W3C
error codes), its message, the browser-side stack trace, and the specification's optional `data`
object — which is what holds the prompt's text on an `unexpected alert open`, and so the only
part of that error a caller can act on. An unrecognized code is kept
verbatim as `Other(code)` rather than being lost, and a reply which is not the expected shape at
all — a crashed driver, an interposed proxy — still arrives as a `WebDriver.Error` carrying the
raw body.

## Status

Tarantula is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Tarantula is designed to be _small_. Its entire source code currently consists
of 183 lines of code.

## Building

Tarantula will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Tarantula?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Tarantula's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Tarantula and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `tarantula`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Tarantula's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Tarantula are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/tarantula/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Tarantula
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Tarantula was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

__Tarantulas__ are spiders, known for making webs, and Tarantula is a library for the WebDriver protocol.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo represents the eight legs of a tarantula.

## License

Tarantula is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

