## Web Automation

### About

Driving a browser and scripting a page are two sides of web automation, and Soundness covers both.
A browser is driven from Scala through the [WebDriver](https://www.w3.org/TR/webdriver2/) protocol:
Chrome or Firefox is launched, a session opened, pages navigated, and elements found, clicked and
typed into — the machinery of end-to-end testing. And browser-side behaviour is written as *typed*
JavaScript: DOM expressions checked against the browser's own
[WebIDL](https://en.wikipedia.org/wiki/Web_IDL) interface definitions as the code compiles,
rendered to JavaScript for a page's event handlers.

### On automating the web

Browser automation scripts are stringly typed twice over: elements are located by selector strings,
and injected behaviour is JavaScript in string literals, unchecked until the browser runs it. A
misspelled DOM method — `getElementByld` — is exactly the kind of mistake a compiler exists to
catch, and exactly the kind these strings hide.

Soundness types both layers: element location goes through the same typed selectors and validated
identifiers used by [CSS](css.md) and [HTML](html.md), and DOM scripting is checked against the
actual interface definitions of the DOM, so only methods that exist, with arguments of the right
types, can be written. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Driving a browser

A browser session launches the driver, opens a session, and closes both when the block ends. Within
it, the session navigates and queries:

```scala
Chrome.session(port = 4444):
  browser.navigateTo(url"https://example.com/")
  browser.title()
```

`Firefox` works identically; each drives its standard driver executable.

### Finding and using elements

Elements are located by typed criteria — a CSS selector list, a validated DOM id, a class list, or
an HTML tag — with `/` returning every match and `element` exactly one. Elements found within
elements descend the tree:

```scala
val heading = browser.element(H1)          // by tag
val items = browser / SelectorList.read(t"nav li")   // by CSS selector

heading.click()
field.value(t"search terms")               // type into an input
button.screenshot()                        // a Raster in Png of the element
```

### Typed page scripting

Behaviour attached to a page — an `onclick`, an `onchange` — is written as a typed DOM expression
rather than a JavaScript string. The `document` and `window` values navigate the DOM's real
interfaces, checked against WebIDL, and render to JavaScript where an [HTML](html.md) attribute
expects a script:

```scala
Button(onclick = document.getElementById(t"foo").focus())
// renders onclick="document.getElementById('foo').focus()"
```

A method the DOM does not define, or an argument of the wrong type, is a compile error — the
JavaScript that reaches the page is known to be well-formed against the interfaces it calls.

### Where each fits

The two halves meet in testing: a page whose handlers were generated from checked expressions is
exercised by a driver whose locators are checked selectors, so the whole round trip — generate,
serve, drive, assert — stays inside the type system, with the browser as the only unchecked
component.
