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
import WebDriver.Session.Action.*

val control = t"\uE009"

browser.perform(Source.Key, List(KeyDown(control), KeyDown(t"a"), KeyUp(t"a"), KeyUp(control)))
browser.releaseActions()
```
The specification assigns each non-typing key a codepoint in Unicode's private use area —
`U+E009` is the control key — so a chord is just a sequence of key-downs and key-ups.
`releaseActions()` lets go of anything an earlier `perform` left held.

### Waiting

Rather than sleeping, set an implicit wait once and let each find retry until the page settles:
```scala
browser.timeouts(WebDriver.Session.Timeouts(`implicit` = 5000L))
```

### Windows, frames and prompts

`window()` gives the current window's handle and `windows()` all of them; `newWindow()` opens
one and returns its handle without switching to it, and `switchTo` moves between them. Frames
are entered with `frame(element)` or `frame(index)`, and left with `parentFrame()` or
`topFrame()`.

A user prompt blocks every other command until it is dealt with, so `alertText()`,
`acceptAlert()` and `dismissAlert()` handle it. `cookies()`, `addCookie`, `deleteCookie` and
`deleteCookies()` manage the cookie jar.

### Running scripts

`execute` runs JavaScript in the page and returns its result as `Json`; `executeAsync` runs a
script that calls a completion callback. This is the escape hatch for anything the protocol
does not model.

### Errors

Every failure raises `WebDriver.Error`, carrying the reason the driver reported (one of the W3C
error codes), its message, and the browser-side stack trace. An unrecognized code is kept
verbatim as `Other(code)` rather than being lost, and a reply which is not the expected shape at
all — a crashed driver, an interposed proxy — still arrives as a `WebDriver.Error` carrying the
raw body.
