## Web Automation

### About

Driving a browser and scripting a page are the two sides of web automation, and both are covered
here. A browser is driven through the [WebDriver](https://www.w3.org/TR/webdriver2/) protocol.
Chrome or Firefox is launched, a [session] is opened, pages are navigated, and elements are
found, clicked and typed into, which is the machinery of end-to-end testing.

Behavior on the browser's side is written as *typed* JavaScript. DOM expressions are checked
against the browser's own [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) interface definitions
as the code compiles, and rendered to JavaScript for the event handlers of a page.

### On automating the web

Browser automation scripts are stringly typed twice over. Elements are located by selector
strings, and injected behavior is JavaScript in string literals, unchecked until the browser
runs it. A misspelled DOM method, such as `getElementByld`, is exactly the kind of mistake which
a compiler exists to catch, and exactly the kind which these strings hide.

Here both layers are typed. Elements are located through the same typed selectors and validated
identifiers which [CSS](css.md) and [HTML](html.md) use. DOM scripting is checked against the
actual interface definitions of the DOM, so only methods which exist, with arguments of the
right types, can be written. A session confined to its block, and page scripts checked against
the DOM's own definitions, follow [delimited scopes](../philosophy/delimited-scopes.md) and
[safety by construction](../philosophy/safety-by-construction.md).

Everything comes from the `soundness` package, with internet access, an HTTP client to speak to
the driver, and concurrency for the driver's process in scope:

```scala
import soundness.*

import environments.javaBaseEnvironment
import errorDiagnostics.stackTracesDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
```

The samples which drive a browser are compiled but not run.

### Driving a browser

A browser session launches the driver, opens a session, and closes both when the block ends,
whether it returns or throws. Within it, the session navigates and queries:

```scala fuel
typecheck
##
supervise:
  WebDriver.Chrome.headless.session:
    browser.navigateTo(url"https://example.com/")
    val title: Text = browser.title()
```

`WebDriver.Firefox`, `WebDriver.Safari` and `WebDriver.Edge` work identically, and each drives
its standard driver executable. `WebDriver(url"http://localhost:4444", capabilities)` drives a
driver which is already running, including a remote grid, through the same interface.

The session handle is a [capability], confined by capture checking to the block which owns it.
Returning it, or anything which captures it, is a compile error, since a session which has been
closed is not something a program should be able to hold. Element handles, by contrast, are
pure values. But every operation on one needs the session in scope, so they are inert outside
it.

### Finding and using elements

Elements are located by typed criteria. `\` returns every match, and `element` exactly one:

```tabular
column  Locator
column  Written as
column  Matches by
row  an HTML tag  `H1`, `Button`  tag name
row  a CSS selector list  `SelectorList.parse(t"nav li")`  CSS selector
row  a validated DOM id  `Name[DomId](t"query")`  CSS selector, the id prefixed with `#`
row  a class list  `ClassList["checkbox"]()`  CSS selector, each class prefixed with `.`
row  an XPath  `xp"//li"`  XPath
row  a `Text`  `t"Sign in"`  link text
```

Elements found within elements descend the tree, and each offers the operations which a test
needs:

```scala fuel
typecheck
##
import htmlDoms.whatwg
import whatwg.*

supervise:
  WebDriver.Chrome.headless.session:
    val heading = browser.element(H1)
    val items = browser \ SelectorList.parse(t"nav li")
    val field = browser.element(Name[DomId](t"query"))
    val button = browser.element(Button)

    heading.click()
    val text: Text = heading.innerText()
    field.value(t"search terms")
    val live: Optional[Text] = field.property(t"value")
    val picture: Raster in Png = button.screenshot()
```

`property` reads the live DOM property, and not the markup. These are the operations on an
element, and all of them need the session in scope:

```tabular
column  Operation
column  Does
row  `click()`, `clear()`  clicks the element, or clears an input
row  `value(text)`  types into an input
row  `innerText()`  reads the rendered text
row  `attribute(name)`, `property(name)`  reads an attribute of the markup, or a live DOM property, as an `Optional`
row  `cssValue(name)`  reads a computed CSS value
row  `tagName()`, `role()`, `label()`  read the element's tag, accessible role and accessible label
row  `enabled()`, `selected()`, `displayed()`  report the element's state
row  `rect()`  reads the element's position and size
row  `screenshot()`, `screenshotData()`  capture the element as a `Raster in Png`, or as PNG bytes
row  `element(locator)`, `\`  find elements within it
row  `shadowRoot()`  reaches its open shadow root
```

Anything which a locator cannot express is reached with `execute`, which runs JavaScript in the
page and returns its result as `Json`. Modifiers, chords, hovers and drags go through `perform`
and the typed actions API. A failure of any kind arrives as a `WebDriver.Error`, which carries
the W3C error code the driver reported, its message, and the stack trace from the browser's
side.

### Waiting

Most of what makes browser automation flaky is timing, and the protocol helps with only part of
it. WebDriver's *implicit* timeout covers find commands, and is set once for the session:

```scala fuel
typecheck
##
supervise:
  WebDriver.Chrome.headless.session:
    browser.timeouts(WebDriver.Session.Timeouts(`implicit` = 5000L))
```

Nothing else has a server-side notion of waiting. For a button which becomes enabled, or a
heading whose text changes, three methods poll under the retry policy in scope:

```tabular
column  Method
column  Waits until
column  Returns
row  `awaitElement(locator)`  an element matches  the first match
row  `awaitElements(locator)`  any element matches  every match
row  `awaitUntil(condition)`  a condition holds  nothing
```

```scala fuel
typecheck
##
import retryTenacities.exponentialTenTimesTenacity

supervise:
  WebDriver.Chrome.headless.session:
    browser.awaitElement(Name[DomId](t"result"))
    browser.awaitUntil(browser.title() == t"Done")
```

The policy is a `Tenacity`, which is the same [retry schedule](concurrency.md#retrying) used
elsewhere, so the interval and the number of attempts belong to the caller. Exhausting them
raises a `Tenacity.Error`. These methods poll the plural endpoint, which reports absence as an
empty list and not as an error. That is deliberate. The error [strategy] of a session is fixed
when it opens, so a failure raised inside the polled block could not be caught here to drive
the next attempt.

### Typed page scripting

Behavior attached to a page, such as an `onclick` or an `onchange`, is written as a typed DOM
expression, and not as a JavaScript string. The `document` and `window` values navigate the real
interfaces of the DOM, checked against WebIDL, and render to JavaScript where an
[HTML](html.md) attribute expects a script:

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import htmlDoms.html4Transitional

Out.println(html4Transitional.Button(onclick = document.getElementById(t"foo").focus()).show)
```

A method which the DOM does not define, or an argument of the wrong type, is a compile error:

```scala fuel
compile
errors show
##
document.getElementByld(t"foo")
```

The JavaScript which reaches the page is known to be well-formed against the interfaces it
calls.

### Where each fits

The two halves meet in testing. A page whose handlers were generated from checked expressions is
exercised by a driver whose locators are checked selectors. The whole round trip, which is to
generate, serve, drive and assert, stays inside the type system, with the browser as the only
unchecked component.

### Related

- [HTML](html.md) and [CSS](css.md) describe the typed documents, selectors and identifiers
  which locators and handlers share.
- [Browser DOM](dom.md) describes scripting which runs in the browser itself, under Scala.js.
- [Foreign interoperability](foreign-interop.md) explains how the WebIDL definitions are read.
- [Testing](testing.md) hosts the end-to-end tests which drive a browser.
- [Concurrency](concurrency.md#retrying) describes the `Tenacity` which governs waiting.
- [Images](images.md) describes the `Raster in Png` which a screenshot gives.
